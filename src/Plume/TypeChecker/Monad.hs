{-# LANGUAGE AllowAmbiguousTypes #-}

module Plume.TypeChecker.Monad
  ( module Monad,
    MonadChecker,
    Checker,
    Placeholder,
    Substitution,
    fetchPositionIO,
    modify,
    get,
    put,
    gets,
    local,
    throw,
    throwRaw,
    insertWith,
    searchEnv,
    insertEnv,
    addClassInstance,
    addClass,
    insertEnvWith,
    deleteEnv,
    deleteManyEnv,
    replaceEnv,
    fresh,
    instantiate,
    instantiateWithSub,
    fetchPosition,
    trackPosition,
    withPosition,
    localPosition,
    getPosition,
    maybeM,
    extractFromArray,
    enterLevel,
    exitLevel,
    removeLink,
    removeNewEntries,
    interpretError,
    tryOr,
  )
where

import Control.Monad.Except
import Control.Monad.Exception (IOThrowable (showErrorIO), compilerError)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Text qualified as T
import GHC.Records
import Plume.Syntax.Parser.Lexer qualified as L
import Plume.Syntax.Concrete (Position)
import Plume.TypeChecker.Monad.Error as Monad
import Plume.TypeChecker.Monad.State as Monad
import Plume.TypeChecker.Monad.Type as Monad
import Plume.TypeChecker.TLIR qualified as Typed
import System.IO.Pretty (ppFailure, printErrorFromString)
import Prelude hiding (get, gets, local, modify, put)

-- | Checker monad transformer, used to handle the type-checking process
-- | with the ability to throw errors.
type MonadChecker m = (MonadIO m, MonadError PlumeError m)

type Checker a = forall m. (MonadChecker m) => m a

tryOr :: (MonadChecker m) => m a -> m a -> m a
tryOr a b = catchError a (const b)

type Placeholder = ReaderT (PlumeQualifier -> Typed.Expression) IO

modify :: (MonadChecker m) => (CheckState -> CheckState) -> m ()
modify f = do
  old <- get
  put (f old)

get :: (MonadChecker m) => m CheckState
get = readIORef checkState

put :: (MonadChecker m) => CheckState -> m ()
put = writeIORef checkState

gets :: (MonadChecker m) => (CheckState -> a) -> m a
gets f = f <$> get

local :: (MonadChecker m) => (CheckState -> CheckState) -> m a -> m a
local f action = do
  old <- get
  put (f old)
  a <- action
  new <- get
  modify $ \s -> s { isAsynchronous = isAsynchronous new}
  pure a

-- | Throwing an error onto the error stack if a position is
-- | found, otherwise print the error and exit the program.
throw :: (MonadChecker m) => TypeError -> m a
throw e = do
  pos <- readIORef checkState <&> positions
  case viaNonEmpty last pos of
    Nothing -> liftIO $ do
      ppFailure (show e)
      exitFailure
    Just p -> throwError (p, e)

-- | Throwing a raw error (without fetching position) onto
-- | the error stack.
throwRaw :: (MonadChecker m) => PlumeError -> m a
throwRaw = throwError

-- | Search for a key in a polymorphic record field
-- | Equivalent to `Map.lookup` but for records.
search ::
  forall l k r a.
  (HasField l r (Map k a), Ord k) =>
  k ->
  r ->
  Maybe a
search key record = do
  let env = getField @l record
  Map.lookup key env

-- | Insert a key-value pair into a polymorphic record field
-- | Equivalent to `Map.insert` but for records.
-- | This function is completely pure and does not modify the record.
insert ::
  forall l k r a.
  (HasField l r (Map k a), Ord k) =>
  k ->
  a ->
  r ->
  r
insert key value record = do
  let env = getField @l record
  setField @l record (Map.insert key value env)

-- | Insert a key-value pair into a polymorphic record field
-- | with a function to combine the new value with the existing one.
insertWith ::
  forall l a m.
  (HasField l CheckState a, MonadChecker m) =>
  (a -> a -> a) ->
  a ->
  m ()
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
searchEnv ::
  forall l a m.
  (HasField l Environment (Map Text a), MonadChecker m) =>
  Text ->
  m (Maybe a)
searchEnv name = do
  env <- get <&> environment
  pure $ search @l name env

-- | Insert a key-value pair into the environment
-- | Equivalent to `Map.insert` but for the environment.
-- | This function modifies the environment in the state.
insertEnv ::
  forall l a m.
  (HasField l Environment (Map Text a), MonadChecker m) =>
  Text ->
  a ->
  m ()
insertEnv name value = do
  env <- get <&> environment
  let rec' = insert @l name value env
  modify $ \s -> setField @"environment" s rec'

addClassInstance :: (MonadChecker m) => (PlumeQualifier, Instance Typed.Expression ()) -> m ()
addClassInstance (q, i) = do
  env <- get <&> environment
  let (MkExtendEnv clsEnv) = getField @"extendEnv" env
  let rec' = setField @"extendEnv" env (MkExtendEnv (update q clsEnv i))
  modify $ \s -> setField @"environment" s rec'

update :: (Eq a) => a -> [(a, b)] -> b -> [(a, b)]
update key [] value = [(key, value)]
update key ((k, v) : xs) value
  | key == k = (key, value) : xs
  | otherwise = (k, v) : update key xs value

addClass :: (MonadChecker m) => Text -> Class -> m ()
addClass name cls = do
  env <- get <&> environment
  let MkClassEnv clsEnv = getField @"classEnv" env
  let rec' = setField @"classEnv" env (MkClassEnv (Map.insert name cls clsEnv))
  modify $ \s -> setField @"environment" s rec'

-- | Delete a key from the environment
-- | Equivalent to `Map.delete` but for the environment.
-- | This function modifies the environment in the state.
deleteEnv ::
  forall l a m.
  (HasField l Environment (Map Text a), MonadChecker m) =>
  Text ->
  m ()
deleteEnv name = do
  env <- get <&> environment
  let rec' = setField @l env (Map.delete name (getField @l env))
  modify $ \s -> setField @"environment" s rec'

deleteManyEnv ::
  forall l a m.
  (HasField l Environment (Map Text a), MonadChecker m) =>
  [Text] ->
  m ()
deleteManyEnv names = do
  env <- get <&> environment
  let rec' = setField @l env (foldr Map.delete (getField @l env) names)
  modify $ \s -> setField @"environment" s rec'

-- | Replace the environment with a new one
-- | This function modifies the environment in the state.
replaceEnv ::
  forall l a m.
  (HasField l Environment (Map Text a), MonadChecker m) =>
  Map Text a ->
  m ()
replaceEnv value = do
  env <- get <&> environment
  modify $ \s -> setField @"environment" s (setField @l env value)

-- | Inserting new values in a polymorphic record field with a function
-- | to combine the new value with the existing one.
insertEnvWith ::
  forall l a m.
  (HasField l Environment (Map Text a), MonadChecker m) =>
  (Map Text a -> Map Text a -> Map Text a) ->
  Map Text a ->
  m ()
insertEnvWith f value = do
  env <- get <&> environment
  let env' = getField @l env
  modify $ \s ->
    setField @"environment" s (setField @l env (f value env'))

genSymbol :: (MonadChecker m) => m Text
genSymbol = do
  s <- readIORef currentSymbol
  writeIORef currentSymbol (s + 1)
  if s < 26
    then pure $ T.singleton (chr (s + 97))
    else pure $ "t" <> show s

enterLevel :: (MonadChecker m) => m ()
enterLevel = modifyIORef' currentLevel (+ 1)

exitLevel :: (MonadChecker m) => m ()
exitLevel = modifyIORef' currentLevel (\x -> x - 1)

-- | Generate a fresh type variable
fresh :: (MonadChecker m) => m PlumeType
fresh = do
  s <- genSymbol
  lvl <- readIORef currentLevel
  ref <- newIORef (Unbound s lvl)
  pure $ TypeVar ref

fetchPositionIO :: MonadIO m => m Position
fetchPositionIO = do
  pos <- readIORef checkState <&> positions
  case viaNonEmpty last pos of
    Nothing -> do
      defaultPos <- readIORef L.defaultPosition
      case defaultPos of
        Nothing -> liftIO $ compilerError "No position found in checker state"
        Just p -> pure p
    Just p -> pure p

type Substitution = Map Text PlumeType

instantiate :: (MonadChecker m) => PlumeScheme -> m (PlumeType, [PlumeQualifier])
instantiate t = do
  (ty, qs, _) <- instantiateWithSub mempty t
  pure (ty, qs)

-- | instantiation: replace schematic variables with fresh TVar
instantiateWithSub :: (MonadChecker m) => Substitution -> PlumeScheme -> m (PlumeType, [PlumeQualifier], Substitution)
instantiateWithSub s (Forall qvars (gens :=>: ty)) = do
  sub <- Map.fromList <$> mapM (\x -> (x,) <$> fresh) qvars
  let s' = Map.union sub s
  (gens', s1) <- goGens s' gens
  (res, s2) <- go s1 ty
  pure (res, gens', s2)
  where
    go :: (MonadChecker m) => Substitution -> PlumeType -> m (PlumeType, Substitution)
    go subst (TypeQuantified name) = case Map.lookup name subst of
      Just t -> pure (t, subst)
      Nothing -> pure (TypeQuantified name, subst)
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

    goMany :: (MonadChecker m) => Substitution -> [PlumeType] -> m ([PlumeType], Substitution)
    goMany subst (x : xs) = do
      (x', subst') <- go subst x
      (xs', subst'') <- goMany subst' xs
      pure (x' : xs', subst'')
    goMany subst [] = pure ([], subst)

    goGens :: (MonadChecker m) => Substitution -> [PlumeQualifier] -> m ([PlumeQualifier], Substitution)
    goGens subst (IsIn name t : xs) = do
      (name', subst') <- go subst name
      (xs', subst'') <- goGens subst' xs
      pure (IsIn name' t : xs', subst'')
    goGens subst (_ : xs) = goGens subst xs
    goGens subst [] = pure ([], subst)

-- | Fetch the current position from the position stack
fetchPosition :: (MonadChecker m) => m Position
fetchPosition = do
  pos <- get <&> positions
  case viaNonEmpty last pos of
    Nothing -> do
      defaultPos <- readIORef L.defaultPosition
      case defaultPos of
        Nothing -> compilerError "No position found in checker state"
        Just p -> pure p
    Just p -> pure p

-- | Track the current position in the position stack
-- | Used to keep track of the position of the current expression
trackPosition :: (MonadChecker m) => Position -> m a -> m a
trackPosition pos action = do
  modify $ \s -> s {positions = positions s ++ [pos]}
  action

-- | Tracking the current position but only for the duration of the action
withPosition :: (MonadChecker m) => Position -> m a -> m a
withPosition pos action = do
  oldPos <- get <&> positions
  modify $ \s -> s {positions = positions s ++ [pos]}
  a <- action
  modify $ \s -> s {positions = oldPos}
  pure a

-- | Localizing the nested positions of the action and then
-- | restoring the old positions after the action is done
localPosition :: (MonadChecker m) => m a -> m a
localPosition action = do
  oldPos <- get <&> positions
  a <- action
  modify $ \s -> s {positions = oldPos}
  pure a

getPosition :: (MonadChecker m) => m a -> m (Position, a)
getPosition action = do
  a <- action
  pos <- fetchPosition
  pure (pos, a)

-- | A monadic version of the `maybe` function
-- | If the maybe value is `Just`, apply the function to the value
-- | and wrap it in a `Just`, otherwise return `Nothing`.
maybeM :: (Applicative f) => Maybe t -> (t -> f a) -> f (Maybe a)
maybeM (Just x) f = Just <$> f x
maybeM Nothing _ = pure Nothing

-- | Extract a single element from a list
-- | Throw an error if the list does not contain a single element
extractFromArray :: (MonadChecker m) => m (b, [a], c) -> m (b, a, c)
extractFromArray ls = do
  (t, xs, h) <- ls
  case xs of
    [x] -> pure (t, x, h)
    r ->
      throw $
        CompilerError $
          "Expected a single element, received: " <> show (length r)

-- | Remove new entries between the "old" `xs` list and the "new" `ys` list
removeNewEntries :: (Eq a) => [a] -> [a] -> [a]
removeNewEntries xs ys = xs L.\\ (xs L.\\ ys)

removeLink :: (MonadChecker m) => PlumeType -> m PlumeType
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

interpretError :: PlumeError -> IO ()
interpretError (p, UnificationFail t1 t2) =
  printErrorFromString
    mempty
    ( "Expected type "
        <> show t1
        <> " but received type "
        <> show t2,
      Nothing,
      p
    )
    "while performing typechecking"
interpretError (p, InfiniteType i t) =
  printErrorFromString
    mempty
    ( "Infinite type "
        <> show i
        <> " in type "
        <> pp,
      Just (show i <> " may occur in the type " <> pp),
      p
    )
    "while performing typechecking"
  where
    pp = show t
interpretError (p, UnificationMismatch t t1' t2') =
  printErrorFromString
    mempty
    ( getMissingError t1' t2',
      Nothing,
      p
    )
    "while performing typechecking"
  where
    getMissingError ts1 ts2
      | null ts1 = "Type " <> show t <> " has no arguments, but should have" <> show (length ts2) <> " arguments"
      | length ts1 < length ts2 = "Expected more arguments, got " <> show (length ts1) <> " but expected " <> show (length ts2)
      | otherwise = "Expected less arguments, got " <> show (length ts1) <> " but expected " <> show (length ts2)
interpretError (p, EmptyMatch) =
  printErrorFromString
    mempty
    ( "Empty switch expression",
      Just "Switch expression must have at least one branch",
      p
    )
    "while performing typechecking"
interpretError (p, UnboundVariable v) =
  printErrorFromString
    mempty
    ( "Unbound variable " <> show v,
      Nothing,
      p
    )
    "while performing typechecking"
interpretError (p, CompilerError e) =
  printErrorFromString
    mempty
    ( "Compiler error: " <> toString e,
      Just "Please report this error to the Plume developers",
      p
    )
    "while performing typechecking"
interpretError (p, DuplicateNative n s) =
  printErrorFromString
    mempty
    ( "Native function " <> toString n <> " with signature " <> show s <> " already defined",
      Nothing,
      p
    )
    "while performing typechecking"
interpretError (p, NoReturnFound t) =
  printErrorFromString
    mempty
    ( "No return found in the expression for type " <> show t,
      Just "Every function must have a return in its body",
      p
    )
    "while performing typechecking"
interpretError (p, ExhaustivenessError hints) =
  printErrorFromString
    mempty
    ( "Non-exhaustive pattern match",
      Just (if null hints then "All possible cases must be covered in the switch expression" else hints),
      p
    )
    "while performing typechecking"
interpretError (p, UnresolvedTypeVariable as) =
  printErrorFromString
    mempty
    ( "Unresolved extensions: " <> showAssumps as,
      Nothing,
      p
    )
    "while performing typechecking"
  where
    showAssumps [] = ""
    showAssumps [x] = showAssump x
    showAssumps (x : xs) = 
      showAssump x <> ", " <> showAssumps xs

    showAssump (_ :>: TypeApp (TypeId tcName) [ty]) = 
      toString (T.drop 1 tcName) <> " for " <> show ty
    showAssump (_ :>: TypeId tcName) = 
      toString (T.drop 1 tcName)
    showAssump _ = error "Not a type application"
interpretError (p, AlreadyDefinedInstance n t) =
  printErrorFromString
    mempty
    ( "Instance " <> toString n <> " already defined for " <> show t,
      Nothing,
      p
    )
    "while performing typechecking"
interpretError (p, ClassMismatch n q1 q2) =
  printErrorFromString
    mempty
    ( "Interface mismatch header for " <> toString n <> ": " <> show q1 <> " and " <> show q2,
      Nothing,
      p
    )
    "while performing typechecking"
interpretError (p, MissingExtensionMethods n ms) =
  printErrorFromString
    mempty
    ( "Missing methods for " <> toString n <> ": " <> intercalate ", " (map toString ms),
      Nothing,
      p
    )
    "while performing typechecking"
interpretError (p, FunctionAlreadyExists n s) =
  printErrorFromString
    mempty
    ( "Function " <> toString n <> " already exists with type " <> show s,
      Nothing,
      p
    )
    "while performing typechecking"
interpretError (p, UnknownExtensionMethods n ms) = 
  printErrorFromString
    mempty
    ( "Unknown methods for " <> toString n <> ": " <> intercalate ", " (map toString ms),
      Nothing,
      p
    )
    "while performing typechecking"

instance IOThrowable PlumeError where
  showErrorIO e = interpretError e >> exitFailure
