{-# LANGUAGE LambdaCase #-}
module Plume.TypeChecker.Constraints.Unification where

import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Monad
import Plume.TypeChecker.TLIR qualified as Typed
import Plume.Syntax.Common.Annotation qualified as Cmm
import qualified Data.Map as Map
import Data.Foldable (foldrM)

doesUnifyWith :: PlumeType -> PlumeType -> IO Bool
doesUnifyWith t t' = do
  t1 <- compressPaths t
  t2 <- compressPaths t'
  doesUnifyWithH t1 t2

doesQualUnifiesWith :: PlumeQualifier -> PlumeQualifier -> IO Bool
doesQualUnifiesWith (IsIn t n) (IsIn t' n') | n == n' = do
  t1 <- mapM compressPaths t
  t2 <- mapM compressPaths t'
  and <$> zipWithM doesUnifyWithH' t1 t2
doesQualUnifiesWith _ _ = pure False

doesQualUnifiesWithE :: PlumeQualifier -> PlumeQualifier -> IO Bool
doesQualUnifiesWithE (IsIn t n) (IsIn t' n') | n == n' = do
  t1 <- mapM compressPaths t
  t2 <- mapM compressPaths t'
  and <$> zipWithM doesUnifyWithH t1 t2
doesQualUnifiesWithE _ _ = pure False

unifyAndGetSub :: MonadChecker m => PlumeType -> PlumeType -> m Substitution
unifyAndGetSub t t' = do
  t1 <- liftIO $ compressPaths t
  t2 <- liftIO $ compressPaths t'
  unifyAndGetSubH t1 t2

composeSubs :: [Substitution] -> IO Substitution
composeSubs = foldrM compose Map.empty

compose :: Substitution -> Substitution -> IO Substitution
compose s1 s2 = do
  -- Map.map (apply s1) s2 `Map.union` s1
  s1' <- mapM (apply s1) s2
  pure $ s1' `Map.union` s1

applyQual :: Substitution -> PlumeQualifier -> IO PlumeQualifier
applyQual s (IsIn t n) = IsIn <$> mapM (apply s) t <*> pure n
applyQual _ q = pure q

apply :: Substitution -> PlumeType -> IO PlumeType
apply _ (TypeId n) = pure $ TypeId n
apply s (TypeApp t ts) = TypeApp <$> apply s t <*> mapM (apply s) ts
apply s (TypeVar u) = do
  v <- readIORef u
  case v of
    Link t -> apply s t
    Unbound t _ -> case Map.lookup t s of
      Just t' -> apply s t'
      Nothing -> pure $ TypeVar u
apply s (TypeQuantified q) = case Map.lookup q s of
  Just t -> apply s t
  Nothing -> pure $ TypeQuantified q

unifyAndGetSubH :: MonadChecker m => PlumeType -> PlumeType -> m Substitution
unifyAndGetSubH t1 t2 | t1 == t2 = pure mempty
unifyAndGetSubH (TypeVar tv) t = do
  tv' <- readIORef tv
  case tv' of
    Link t' -> unifyAndGetSub t' t
    Unbound qv _ -> pure (Map.singleton qv t)
unifyAndGetSubH t (TypeVar tv) = do
  tv' <- readIORef tv
  case tv' of
    Link t' -> unifyAndGetSub t t'
    Unbound qv _ -> pure (Map.singleton qv t)
unifyAndGetSubH (TypeApp t1 t1') (TypeApp t2 t2') | length t1' == length t2' = do
  s1 <- unifyAndGetSub t1 t2
  s2 <- foldrM (\(t1'', t2'') s -> do
    s' <- unifyAndGetSub t1'' t2''
    liftIO $ compose s' s
    ) mempty (zip t1' t2')
  pure (s1 <> s2)
unifyAndGetSubH (TypeQuantified q1) t = do
  b <- liftIO $ doesOccurQ q1 t
  if b
    then pure mempty
    else pure (Map.singleton q1 t)
unifyAndGetSubH t (TypeQuantified q2) = do
  b <- liftIO $ doesOccurQ q2 t
  if b
    then pure mempty
    else pure (Map.singleton q2 t)
unifyAndGetSubH _ _ = error "Unification failed"

doesUnifyWithScheme :: PlumeType -> PlumeScheme -> IO Bool
doesUnifyWithScheme t (Forall _ (_ :=>: t')) = do
  t1 <- compressPaths t
  t2 <- compressPaths t'
  doesUnifyWithH t1 t2

doesUnifyWithH :: PlumeType -> PlumeType -> IO Bool
doesUnifyWithH t1 t2 | t1 == t2 = pure True
doesUnifyWithH (TypeId "variable") (TypeId "list") = pure True
doesUnifyWithH (TypeId "list") (TypeId "variable") = pure True
doesUnifyWithH (TypeVar tv) t = do
  tv' <- readIORef tv
  case tv' of
    Link t' -> doesUnifyWith t' t
    Unbound _ _ -> pure True
doesUnifyWithH t (TypeVar tv) = do
  tv' <- readIORef tv
  case tv' of
    Link t' -> doesUnifyWith t t'
    Unbound _ _ -> pure True
doesUnifyWithH (TypeApp t1 t1') (TypeApp t2 t2') | length t1' == length t2' =
  and <$> zipWithM doesUnifyWith (t1:t1') (t2:t2')
doesUnifyWithH (TypeQuantified q1) (TypeQuantified q2) = pure (q1 == q2)
doesUnifyWithH (TypeQuantified q1) t = not <$> doesOccurQ q1 t
doesUnifyWithH t (TypeQuantified q2) = not <$> doesOccurQ q2 t
doesUnifyWithH _ _ = pure False

doesUnifyWith' :: PlumeType -> PlumeType -> IO Bool
doesUnifyWith' t1 t2 = do
  t1' <- compressPaths t1
  t2' <- compressPaths t2
  doesUnifyWithH' t1' t2'

doesUnifyWithH' :: PlumeType -> PlumeType -> IO Bool
doesUnifyWithH' t1 t2 | t1 == t2 = pure True
doesUnifyWithH' (TypeId "variable") (TypeId "list") = pure True
doesUnifyWithH' (TypeId "list") (TypeId "variable") = pure True
doesUnifyWithH' (TypeVar tv) t = do
  tv' <- readIORef tv
  case tv' of
    Link t' -> doesUnifyWith' t' t
    Unbound _ _ -> pure True
doesUnifyWithH' t (TypeVar tv) = do
  tv' <- readIORef tv
  case tv' of
    Link t' -> doesUnifyWith' t t'
    Unbound _ _ -> pure True
doesUnifyWithH' (TypeApp t1 t1') (TypeApp t2 t2') | length t1' == length t2' =
  and <$> zipWithM doesUnifyWith' (t1:t1') (t2:t2')
doesUnifyWithH' (TypeQuantified _) (TypeQuantified _) = pure True
doesUnifyWithH' (TypeQuantified q1) t = not <$> doesOccurQ q1 t
doesUnifyWithH' t (TypeQuantified q2) = not <$> doesOccurQ q2 t
doesUnifyWithH' _ _ = pure False

doesOccurQ :: QuVar -> PlumeType -> IO Bool
doesOccurQ q (TypeVar tv) = do
  tv' <- readIORef tv
  case tv' of
    Link t -> doesOccurQ q t
    Unbound _ _ -> pure False
doesOccurQ q (TypeApp t ts) = do
  b <- doesOccurQ q t
  b' <- or <$> traverse (doesOccurQ q) ts
  pure (b || b')
doesOccurQ _ _ = pure False

-- | Creating a constraint from a type constraint
createConstraint :: MonadChecker m => TypeConstraint -> m PlumeConstraint
createConstraint c = do
  p <- fetchPosition
  pure (p, c)

-- check to see if a TVar (the first argument) occurs in the type
-- given as the second argument. Fail if it does.
-- At the same time, update the levels of all encountered free
-- variables to be the min of variable's current level and
-- the level of the given variable tvr.
doesOccur :: IORef TyVar -> PlumeType -> IO ()
doesOccur tvr (TypeVar tv') = do
  tvr' <- readIORef tvr
  tvr'' <- readIORef tv'
  case tvr'' of
    Link t -> doesOccur tvr t
    Unbound name lvl -> do
      let newMinLvl = case tvr' of
            Link _ -> lvl
            Unbound _ lvl' -> min lvl' lvl
      writeIORef tv' (Unbound name newMinLvl)
doesOccur tv (TypeApp t1 t2) = do
  doesOccur tv t1
  traverse_ (doesOccur tv) t2
doesOccur _ _ = pure ()

mguSch :: MonadChecker m => PlumeType -> PlumeScheme -> m ()
mguSch t (Forall _ (_ :=>: t')) = mgu t t'

mgu :: MonadChecker m => PlumeType -> PlumeType -> m ()
mgu t t' = do
  t1 <- liftIO $ compressPaths t
  t2 <- liftIO $ compressPaths t'
  if t1 == t2
    then pure ()
    else case (t1, t2) of
      (TypeId "variable", TypeId "list") -> pure ()
      (TypeId "list", TypeId "variable") -> pure ()
      (TypeVar tv1, _) -> readIORef tv1 >>= \case
        Link tl -> mgu tl t2
        Unbound _ _ -> liftIO $ do
          doesOccur tv1 t2
          writeIORef tv1 (Link t2)
      (_, TypeVar tv2) -> readIORef tv2 >>= \case
        Link tl -> mgu t1 tl
        Unbound _ _ -> liftIO $ do
          doesOccur tv2 t1
          writeIORef tv2 (Link t1)
      (TypeApp t1a t1b, TypeApp t2a t2b) | length t1b == length t2b -> do
        mgu t1a t2a
        zipWithM_ mgu t1b t2b
      (TypeId n, TypeId n') | n == n' -> pure ()
      _ -> throw (UnificationFail t1 t2)

compressQual :: PlumeQualifier -> IO PlumeQualifier
compressQual (IsIn t n) = IsIn <$> mapM compressPaths t <*> pure n
compressQual q = pure q

compressPaths :: PlumeType -> IO PlumeType
compressPaths (TypeVar tv) = do
  tv' <- readIORef tv
  case tv' of
    Link t -> do
      t' <- compressPaths t
      writeIORef tv (Link t')
      pure t'
    Unbound _ _ -> pure (TypeVar tv)
compressPaths (TypeApp t ts) = do
  t' <- compressPaths t
  ts' <- traverse compressPaths ts
  pure (TypeApp t' ts')
compressPaths t = pure t

-- | Lift a block of expressions to check if any return is present
-- | and if the return type matches the expected return type.
liftBlock :: 
  Placeholder Typed.Expression ->
  [PlumeType] -> 
  PlumeType -> 
  Placeholder Typed.Expression
liftBlock block _ t = do
  f <- ask
  res <- liftIO $ runReaderT block f
  ty <- liftIO $ compressPaths t

  case res of
    Typed.EBlock exprs
      | not (any Typed.containsReturn exprs)
        && isTVar ty -> case ty of
          TypeVar ref -> do
            writeIORef ref (Link TUnit)
            pure $ Typed.EBlock exprs
          _ -> error "Not a type variable"

    Typed.EBlock exprs 
      | any Typed.containsReturn exprs 
        || ty == TUnit
        || ty == TAsync TUnit -> do
          pure $ Typed.EBlock exprs
    
    _ -> pure res
  
  where 
    isTVar (TypeVar _) = True
    isTVar _ = False


-- | Lifting placeholder is used to create a reader monad from a variable name,
-- | a type and a list of qualifiers. It resolves the qualifiers according to a 
-- | given environment and creates the necessary calls.
liftPlaceholders ::
  Text ->
  PlumeType ->
  [PlumeQualifier] ->
  Placeholder Typed.Expression
liftPlaceholders name ty ps = do
  f <- ask
  let dicts = fmap f ps
  pure $ case length dicts of
    0 -> Typed.EVariable (Cmm.fromText name) (Identity ty)
    _ | null dicts -> Typed.EInstanceVariable (Cmm.fromText name) (Identity ty)
    _ -> Typed.EApplication (Typed.EInstanceVariable (Cmm.fromText name) (Identity ty)) dicts