{-# LANGUAGE AllowAmbiguousTypes #-}

module Plume.TypeChecker.Monad (
  module Monad,
  runChecker,
  Checker,
  throw,
  insertWith,
  searchEnv,
  insertEnv,
  insertEnvWith,
  deleteEnv,
  replaceEnv,
  fresh,
  instantiate,
  fetchPosition,
  trackPosition,
  withPosition,
  localPosition,
  maybeM,
  extractFromArray,
  pushConstraint,
  getSubst,
  updateSubst,
) where

import Control.Monad.Except
import Data.Map qualified as Map
import GHC.Records
import Plume.Syntax.Concrete (Position)
import Plume.TypeChecker.Monad.Error as Monad
import Plume.TypeChecker.Monad.Free as Monad
import Plume.TypeChecker.Monad.State as Monad
import Plume.TypeChecker.Monad.Type as Monad

newtype CheckerT m a
  = MkChecker {runCheckerT :: ExceptT PlumeError m a}

instance (Functor m) => Functor (CheckerT m) where
  fmap f (MkChecker m) = MkChecker (fmap f m)

instance (Monad m) => Applicative (CheckerT m) where
  pure x = MkChecker (pure x)
  MkChecker f <*> MkChecker x = MkChecker (f <*> x)

instance (Monad m) => Monad (CheckerT m) where
  MkChecker m >>= f = MkChecker (m >>= runCheckerT . f)

type Checker = CheckerT IO

runChecker :: Checker a -> IO (Either PlumeError a)
runChecker = runExceptT . runCheckerT

throw :: TypeError -> Checker a
throw e = MkChecker $ do
  pos <- readIORef checkState <&> positions
  case viaNonEmpty last pos of
    Nothing -> error "No position found in checker state"
    Just p -> throwError (p, e)

instance MonadState CheckState Checker where
  get = MkChecker $ readIORef checkState
  put s = MkChecker $ writeIORef checkState s

instance MonadIO Checker where
  liftIO = MkChecker . liftIO

search
  :: forall l k r a
   . (HasField l r (Map k a), Ord k)
  => k
  -> r
  -> Maybe a
search key record = do
  let env = getField @l record
  Map.lookup key env

insert
  :: forall l k r a
   . (HasField l r (Map k a), Ord k)
  => k
  -> a
  -> r
  -> r
insert key value record = do
  let env = getField @l record
  setField @l record (Map.insert key value env)

insertWith
  :: forall l a
   . (HasField l CheckState a)
  => (a -> a -> a)
  -> a
  -> Checker ()
insertWith f value = do
  modify $ \s -> setField @l s (f value (getField @l s))

pushConstraint
  :: forall l a
   . (HasField l Constraints [a])
  => a
  -> Checker ()
pushConstraint c = do
  modify $ \s ->
    s
      { constraints =
          setField @l
            s.constraints
            (getField @l s.constraints ++ [c])
      }

searchEnv
  :: forall l a
   . (HasField l Environment (Map Text a))
  => Text
  -> Checker (Maybe a)
searchEnv name = do
  env <- get <&> environment
  pure $ search @l name env

insertEnv
  :: forall l a
   . (HasField l Environment (Map Text a))
  => Text
  -> a
  -> Checker ()
insertEnv name value = do
  env <- get <&> environment
  let rec' = insert @l name value env
  modify $ \s -> setField @"environment" s rec'

deleteEnv
  :: forall l a
   . (HasField l Environment (Map Text a))
  => Text
  -> Checker ()
deleteEnv name = do
  env <- get <&> environment
  let rec' = setField @l env (Map.delete name (getField @l env))
  modify $ \s -> setField @"environment" s rec'

replaceEnv
  :: forall l a
   . (HasField l Environment (Map Text a))
  => Map Text a
  -> Checker ()
replaceEnv value = do
  env <- get <&> environment
  modify $ \s -> setField @"environment" s (setField @l env value)

insertEnvWith
  :: forall l a
   . (HasField l Environment (Map Text a))
  => (Map Text a -> Map Text a -> Map Text a)
  -> Map Text a
  -> Checker ()
insertEnvWith f value = do
  env <- get <&> environment
  let env' = getField @l env
  modify $ \s ->
    setField @"environment" s (setField @l env (f value env'))

fresh :: Checker PlumeType
fresh = do
  n <- get <&> nextTyVar
  modify $ \s -> s {nextTyVar = n + 1}
  pure $ TypeVar (MkTyVar n)

instantiate :: PlumeScheme -> Checker PlumeType
instantiate (Forall vars ty) = do
  subst <- Map.fromList <$> traverse (\v -> (v,) <$> fresh) vars
  pure $ apply subst ty

fetchPosition :: Checker Position
fetchPosition = do
  pos <- get <&> positions
  case viaNonEmpty last pos of
    Nothing -> error "No position found in checker state"
    Just p -> pure p

trackPosition :: Position -> Checker a -> Checker a
trackPosition pos action = do
  modify $ \s -> s {positions = positions s ++ [pos]}
  action

withPosition :: Position -> Checker a -> Checker a
withPosition pos action = do
  oldPos <- get <&> positions
  modify $ \s -> s {positions = positions s ++ [pos]}
  a <- action
  modify $ \s -> s {positions = oldPos}
  pure a

localPosition :: Checker a -> Checker a
localPosition action = do
  oldPos <- get <&> positions
  a <- action
  modify $ \s -> s {positions = oldPos}
  pure a

maybeM :: (Applicative f) => Maybe t -> (t -> f a) -> f (Maybe a)
maybeM (Just x) f = Just <$> f x
maybeM Nothing _ = pure Nothing

extractFromArray :: Checker (b, [a]) -> Checker (b, a)
extractFromArray ls = do
  (t, xs) <- ls
  case xs of
    [x] -> pure (t, x)
    r ->
      throw $
        CompilerError $
          "Expected a single element, received: " <> show (length r)

updateSubst :: Substitution -> Checker ()
updateSubst s2 = do
  s1 <- getSubst
  modify $ \st ->
    st {constraints = st.constraints {substitution = s2 <> s1}}

getSubst :: Checker Substitution
getSubst = gets (substitution . constraints)

instance MonadReader CheckState Checker where
  ask = get
  local f action = do
    s <- get
    put (f s)
    a <- action
    modify $ \st ->
      st
        { environment = s.environment
        , returnType = s.returnType
        }
    pure a