module Language.Plume.Frontend.TypeChecking where

import Language.Plume.Frontend.TypeChecking.Monad qualified as M
import Language.Plume.Frontend.TypeChecking.Checker qualified as C
import Language.Plume.Syntax.HLIR qualified as HLIR
import Control.Monad.Result qualified as Err

typecheck
  :: MonadIO m
  => [HLIR.AST "declaration"]
  -> m (Either (Err.PlumeError, HLIR.Position) [HLIR.HLIR "declaration"])
typecheck decls = do
  let initialState = M.MkCheckerState M.defaultVariables mempty Nothing
  writeIORef M.checkerState initialState
  result <- runExceptT $ mapM C.typecheckD decls
  writeIORef M.checkerState initialState

  pure $ second (map fst) result
