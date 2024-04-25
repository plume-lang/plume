{-# LANGUAGE AllowAmbiguousTypes #-}

module Plume.TypeChecker.Monad (
  module Monad,
  runChecker,
  Checker,
  CheckerT(..),
  throw,
  throwRaw,
  insertWith,
  searchEnv,
  insertEnv,
  insertEnvWith,
  deleteEnv,
  deleteManyEnv,
  replaceEnv,
  fresh,
  instantiate,
  fetchPosition,
  trackPosition,
  withPosition,
  localPosition,
  maybeM,
  extractFromArray,
  enterLevel,
  exitLevel,
  removeLink,
  removeExtLink,
  removeNewEntries,
) where

import Control.Monad.Except
import Control.Monad.Exception
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Text.IO qualified as T
import Data.Text qualified as T
import GHC.Records
import Plume.Syntax.Concrete (Position)
import Plume.TypeChecker.Monad.Error as Monad
import Plume.TypeChecker.Monad.State as Monad
import Plume.TypeChecker.Monad.Type as Monad

-- | Checker monad transformer, used to handle the type-checking process
-- | with the ability to throw errors.
newtype CheckerT m a
  = MkChecker {runCheckerT :: ExceptT PlumeError m a}

instance (Functor m) => Functor (CheckerT m) where
  fmap f (MkChecker m) = MkChecker (fmap f m)

instance (Monad m) => Applicative (CheckerT m) where
  pure x = MkChecker (pure x)
  MkChecker f <*> MkChecker x = MkChecker (f <*> x)

instance (Monad m) => Monad (CheckerT m) where
  MkChecker m >>= f = MkChecker (m >>= runCheckerT . f)

-- | Default checker alias for the IO monad
type Checker = CheckerT IO

runChecker :: Checker a -> IO (Either PlumeError a)
runChecker = runExceptT . runCheckerT

-- | Throwing an error onto the error stack if a position is
-- | found, otherwise print the error and exit the program.
throw :: TypeError -> Checker a
throw e = MkChecker $ do
  pos <- readIORef checkState <&> positions
  case viaNonEmpty last pos of
    Nothing -> liftIO $ do
      T.putStrLn (showError e)
      exitFailure
    Just p -> throwError (p, e)

-- | Throwing a raw error (without fetching position) onto
-- | the error stack.
throwRaw :: PlumeError -> Checker a
throwRaw e = MkChecker $ throwError e

-- MonadState, MonadIO and MonadReader instances for the Checker monad

instance MonadState CheckState Checker where
  get = MkChecker $ readIORef checkState
  put s = MkChecker $ writeIORef checkState s

instance MonadIO Checker where
  liftIO = MkChecker . liftIO

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

-- | Search for a key in a polymorphic record field
-- | Equivalent to `Map.lookup` but for records.
search
  :: forall l k r a
   . (HasField l r (Map k a), Ord k)
  => k
  -> r
  -> Maybe a
search key record = do
  let env = getField @l record
  Map.lookup key env

-- | Insert a key-value pair into a polymorphic record field
-- | Equivalent to `Map.insert` but for records.
-- | This function is completely pure and does not modify the record.
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

-- | Insert a key-value pair into a polymorphic record field
-- | with a function to combine the new value with the existing one.
insertWith
  :: forall l a
   . (HasField l CheckState a)
  => (a -> a -> a)
  -> a
  -> Checker ()
insertWith f value = do
  modify $ \s -> setField @l s (f value (getField @l s))

-- | Push a constraint into the constraint stack
-- | Used to generate type constraints during the type-checking process.
-- {-# INLINE pushConstraint #-}
-- pushConstraint
--   :: forall l a
--    . (HasField l Constraints [a])
--   => a
--   -> Checker ()
-- pushConstraint c = do
--   modify $ \s ->
--     s
--       { constraints =
--           setField @l
--             s.constraints
--             (getField @l s.constraints ++ [c])
--       }

-- | Search for a key in the environment
-- | Equivalent to `Map.lookup` but for the environment.
-- | This function does not require passing environment at all, it
-- | fetches the environment from the state.
searchEnv
  :: forall l a
   . (HasField l Environment (Map Text a))
  => Text
  -> Checker (Maybe a)
searchEnv name = do
  env <- get <&> environment
  pure $ search @l name env

-- | Insert a key-value pair into the environment
-- | Equivalent to `Map.insert` but for the environment.
-- | This function modifies the environment in the state.
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

-- | Delete a key from the environment
-- | Equivalent to `Map.delete` but for the environment.
-- | This function modifies the environment in the state.
deleteEnv
  :: forall l a
   . (HasField l Environment (Map Text a))
  => Text
  -> Checker ()
deleteEnv name = do
  env <- get <&> environment
  let rec' = setField @l env (Map.delete name (getField @l env))
  modify $ \s -> setField @"environment" s rec'

deleteManyEnv 
  :: forall l a
   . (HasField l Environment (Map Text a))
  => [Text]
  -> Checker ()
deleteManyEnv names = do
  env <- get <&> environment
  let rec' = setField @l env (foldr Map.delete (getField @l env) names)
  modify $ \s -> setField @"environment" s rec'

-- | Replace the environment with a new one
-- | This function modifies the environment in the state.
replaceEnv
  :: forall l a
   . (HasField l Environment (Map Text a))
  => Map Text a
  -> Checker ()
replaceEnv value = do
  env <- get <&> environment
  modify $ \s -> setField @"environment" s (setField @l env value)

-- | Inserting new values in a polymorphic record field with a function
-- | to combine the new value with the existing one.
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

genSymbol :: Checker Text
genSymbol = do
  s <- readIORef currentSymbol
  writeIORef currentSymbol (s + 1)
  if s < 26
    then pure $ T.singleton (chr (s + 97))
    else pure $ "t" <> show s

enterLevel :: Checker ()
enterLevel = modifyIORef' currentLevel (+ 1)

exitLevel :: Checker ()
exitLevel = modifyIORef' currentLevel (\x -> x - 1)
  
-- | Generate a fresh type variable
fresh :: Checker PlumeType
fresh = do
  s <- genSymbol
  lvl <- readIORef currentLevel
  ref <- newIORef (Unbound s lvl)
  pure $ TypeVar ref

type Substitution = Map Text PlumeType

-- | instantiation: replace schematic variables with fresh TVar
instantiate :: PlumeType -> Checker PlumeType
instantiate = fmap fst <$> go mempty
  where
    go :: Substitution -> PlumeType -> Checker (PlumeType, Substitution)
    go subst (TypeQuantified name) = case Map.lookup name subst of
      Just t -> pure (t, subst)
      Nothing -> do
        t <- fresh
        pure (t, Map.insert name t subst)
    go subst (TypeApp t ts) = do
      (t', subst') <- go subst t
      (ts', subst'') <- goMany subst' ts
      pure (TypeApp t' ts', subst'')
    go subst (TypeVar ref) = do
      v <- readIORef ref
      case v of
        Link t -> go subst t
        Unbound _ _ -> pure (TypeVar ref, subst)
    go subst t = pure (t, subst)

    goMany :: Substitution -> [PlumeType] -> Checker ([PlumeType], Substitution)
    goMany subst (x:xs) = do
      (x', subst') <- go subst x
      (xs', subst'') <- goMany subst' xs
      pure (x':xs', subst'')
    goMany subst [] = pure ([], subst)

-- | Fetch the current position from the position stack
fetchPosition :: Checker Position
fetchPosition = do
  pos <- get <&> positions
  case viaNonEmpty last pos of
    Nothing -> error "No position found in checker state"
    Just p -> pure p

-- | Track the current position in the position stack
-- | Used to keep track of the position of the current expression
trackPosition :: Position -> Checker a -> Checker a
trackPosition pos action = do
  modify $ \s -> s {positions = positions s ++ [pos]}
  action

-- | Tracking the current position but only for the duration of the action
withPosition :: Position -> Checker a -> Checker a
withPosition pos action = do
  oldPos <- get <&> positions
  modify $ \s -> s {positions = positions s ++ [pos]}
  a <- action
  modify $ \s -> s {positions = oldPos}
  pure a

-- | Localizing the nested positions of the action and then
-- | restoring the old positions after the action is done
localPosition :: Checker a -> Checker a
localPosition action = do
  oldPos <- get <&> positions
  a <- action
  modify $ \s -> s {positions = oldPos}
  pure a

-- | A monadic version of the `maybe` function
-- | If the maybe value is `Just`, apply the function to the value
-- | and wrap it in a `Just`, otherwise return `Nothing`.
maybeM :: (Applicative f) => Maybe t -> (t -> f a) -> f (Maybe a)
maybeM (Just x) f = Just <$> f x
maybeM Nothing _ = pure Nothing

-- | Extract a single element from a list
-- | Throw an error if the list does not contain a single element
extractFromArray :: Checker (b, [a]) -> Checker (b, a)
extractFromArray ls = do
  (t, xs) <- ls
  case xs of
    [x] -> pure (t, x)
    r ->
      throw $
        CompilerError $
          "Expected a single element, received: " <> show (length r)

-- | Remove new entries between the "old" `xs` list and the "new" `ys` list 
removeNewEntries :: (Eq a) => [a] -> [a] -> [a]
removeNewEntries xs ys = xs L.\\ (xs L.\\ ys)

removeLink :: PlumeType -> Checker PlumeType
removeLink (TypeVar ref) = do
  v <- readIORef ref
  case v of
    Link t -> removeLink t
    Unbound {} -> pure $ TypeVar ref
removeLink (TypeApp t ts) = do
  t' <- removeLink t
  ts' <- mapM removeLink ts
  pure $ TypeApp t' ts'
removeLink t = pure t

removeExtLink :: Extension -> Checker Extension
removeExtLink (MkExtension name ty scheme) = do
  ty' <- removeLink ty
  scheme' <- removeLink scheme
  pure $ MkExtension name ty' scheme'