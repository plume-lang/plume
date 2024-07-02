module Language.Plume.Backend.Monomorphization.Monad where

import GHC.IO qualified as IO
import Language.Plume.Syntax.MLIR qualified as MLIR
import Data.Map qualified as Map

type MonadMono m = MonadIO m
type Substitution = Map MLIR.QuVar MLIR.PlumeType

{-# NOINLINE symbolCounter #-}
symbolCounter :: IORef Int
symbolCounter = IO.unsafePerformIO $ newIORef 0

{-# NOINLINE resultState #-}
resultState :: IORef [MLIR.MLIR "declaration"]
resultState = IO.unsafePerformIO $ newIORef []

freshSymbol :: MonadMono m => Text -> m Text
freshSymbol n = do
  modifyIORef' symbolCounter (+1)
  i <- readIORef symbolCounter

  pure $ "@" <> n <> "__mono__" <> show i

{-# NOINLINE functionTable #-}
functionTable :: IORef (Map Text (MLIR.PlumeScheme, MLIR.MLIR "declaration"))
functionTable = IO.unsafePerformIO $ newIORef mempty

{-# NOINLINE reserved #-}
reserved :: IORef (Set Text)
reserved = IO.unsafePerformIO $ newIORef mempty

{-# NOINLINE locals #-}
locals :: IORef (Set Text)
locals = IO.unsafePerformIO $ newIORef mempty

withLocals :: MonadMono m => Set Text -> m a -> m a
withLocals l m = do
  old <- readIORef locals
  modifyIORef' locals (<> l)
  r <- m
  writeIORef locals old

  pure r

data MonoUnit = MkMonoUnit
  { name :: Text
  , original :: Text
  , ty :: MLIR.PlumeType
  , subst :: Substitution
  }
  deriving (Eq, Show)

{-# NOINLINE dependencies #-}
dependencies :: IORef [(Text, MonoUnit)]
dependencies = IO.unsafePerformIO $ newIORef mempty

substFromUnify
  :: MonadIO m
  => MLIR.PlumeType
  -> MLIR.PlumeType
  -> m Substitution
substFromUnify (MLIR.MkTyVar v1) t2 = do
  r <- readIORef v1
  case r of
    MLIR.Unbound qv _ -> pure $ Map.singleton qv t2
    MLIR.Link t -> substFromUnify t t2
substFromUnify t1 (MLIR.MkTyVar v2) = do
  r <- readIORef v2
  case r of
    MLIR.Unbound qv _ -> pure $ Map.singleton qv t1
    MLIR.Link t -> substFromUnify t1 t
substFromUnify (MLIR.MkTyApp f1 a1) (MLIR.MkTyApp f2 a2) = do
  s1 <- substFromUnify f1 f2
  s2 <- zipWithM substFromUnify a1 a2

  pure $ s1 <> fold s2
substFromUnify (MLIR.MkTyId n) (MLIR.MkTyId m) | n == m = pure mempty
substFromUnify (MLIR.MkTyExists q1 t1) (MLIR.MkTyExists q2 t2) = do
  s <- substFromUnify t1 t2

  pure $ foldr Map.delete s [q1, q2]
substFromUnify t1 t2 = error $ "substFromUnify: " <> show t1 <> " " <> show t2

apply
  :: MonadIO m
  => Substitution
  -> MLIR.PlumeType
  -> m MLIR.PlumeType
apply s (MLIR.MkTyVar v) = do
  r <- readIORef v
  case r of
    MLIR.Unbound qv _ -> case Map.lookup qv s of
      Just t -> pure t
      Nothing -> pure $ MLIR.MkTyVar v
    MLIR.Link t -> apply s t
apply s (MLIR.MkTyApp f a) = do
  f' <- apply s f
  a' <- mapM (apply s) a

  pure $ MLIR.MkTyApp f' a'
apply s (MLIR.MkTyExists qvar ty) = do
  let newSubst = Map.insert qvar MLIR.MkTyUnit s
  apply newSubst ty
apply _ t = pure t

containsTypeVar :: MonadIO m => MLIR.PlumeType -> m Bool
containsTypeVar (MLIR.MkTyVar v) = do
  r <- readIORef v
  case r of
    MLIR.Unbound _ _ -> pure True
    MLIR.Link t -> containsTypeVar t
containsTypeVar (MLIR.MkTyApp f a) = do
  f' <- containsTypeVar f
  a' <- anyM containsTypeVar a

  pure $ f' || a'
containsTypeVar (MLIR.MkTyExists _ _) = pure True
containsTypeVar _ = pure False
