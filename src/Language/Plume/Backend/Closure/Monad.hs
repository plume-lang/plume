module Language.Plume.Backend.Closure.Monad where

import GHC.IO qualified as IO
import Data.Map qualified as Map
import Language.Plume.Syntax.Internal.Type qualified as Ty
import Language.Plume.Syntax.MLIR qualified as MLIR

type MonadClosure m = (MonadIO m)

{-# NOINLINE symbolCounter #-}
symbolCounter :: IORef Int
symbolCounter = IO.unsafePerformIO $ newIORef 0

{-# NOINLINE returnType #-}
returnType :: IORef (Maybe Ty.PlumeType)
returnType = IO.unsafePerformIO $ newIORef Nothing

{-# NOINLINE reserved #-}
reserved :: IORef (Set Text)
reserved = IO.unsafePerformIO $ newIORef mempty

{-# NOINLINE globals #-}
globals :: IORef (Map Text Ty.PlumeType)
globals = IO.unsafePerformIO $ newIORef mempty

freshSymbol :: MonadClosure m => Text -> m Text
freshSymbol n = do
  modifyIORef' symbolCounter (+1)
  i <- readIORef symbolCounter

  pure $ "@" <> n <> "__closure__" <> show i

freshType :: MonadClosure m => Text -> m Ty.PlumeType
freshType n = do
  name <- freshSymbol n
  var <- newIORef (Ty.Unbound name 0)

  pure $ Ty.MkTyVar var

class Free a where
  free :: a -> Map Text Ty.PlumeType

instance Free (MLIR.Expression Ty.PlumeType) where
  free (MLIR.MkExprCall callee args _) = free callee <> foldMap free args
  free (MLIR.MkExprIf cond then' else') = free cond <> free then' <> free else'
  free (MLIR.MkExprBlock exprs _) = foldMap free exprs
  free (MLIR.MkExprLambda args _ body) = free body Map.\\ free args
  free (MLIR.MkExprLiteral _) = Map.empty
  free (MLIR.MkExprVariable name ty) =  Map.singleton name ty
  free (MLIR.MkExprField expr _ _) = free expr
  free (MLIR.MkExprPack anns (lam, struct) _) = free lam Map.\\ (free anns <> free struct)
  free (MLIR.MkExprLet name ty expr _ body) = (free expr <> free body) Map.\\ Map.singleton name ty
  free (MLIR.MkExprTupleAccess expr _ _) = free expr
  free (MLIR.MkExprClosureCall e args _) = free e <> free args
  free (MLIR.MkExprReturn e) = free e

instance Free a => Free (Maybe a) where
  free = foldMap free

instance Free (MLIR.Annotation Ty.PlumeType) where
  free (MLIR.MkAnnotation name ty) = Map.singleton name ty

instance Free (MLIR.Annotation (MLIR.Expression Ty.PlumeType)) where
  free (MLIR.MkAnnotation _ expr) = free expr

instance Free a => Free [a] where
  free = foldMap free
