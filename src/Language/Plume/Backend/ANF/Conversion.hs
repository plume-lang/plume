module Language.Plume.Backend.ANF.Conversion where
import qualified Language.Plume.Syntax.MLIR as MLIR
import qualified Language.Plume.Backend.ANF.Monad as M
import Control.Monad.Result (compilerError)

buildExprs :: M.MonadANF m => [(MLIR.MLIR "expression", [MLIR.MLIR "expression"])] -> m [MLIR.MLIR "expression"]
buildExprs ((expr, exprs) : xs) = do
  xs' <- buildExprs xs
  pure $ exprs <> [expr] <> xs'
buildExprs [] = pure []

convertE :: M.MonadANF m => MLIR.MLIR "expression" -> m (MLIR.MLIR "expression", [MLIR.MLIR "expression"])
convertE (MLIR.MkExprLiteral lit) = pure (MLIR.MkExprLiteral lit, [])
convertE (MLIR.MkExprVariable name ty) = pure (MLIR.MkExprVariable name ty, [])
convertE (MLIR.MkExprLet name ty expr bodyT next) = do
  (expr', exprs1) <- convertE expr
  res <- traverse convertE next

  case res of
    Just (next', exprs2) -> do
      fresh <- M.freshSymbol "let"

      let finalExprs = 
            exprs1 
              <> [MLIR.MkExprLet name ty expr' MLIR.MkTyUnit Nothing] 
              <> exprs2 
              <> [MLIR.MkExprLet fresh bodyT next' MLIR.MkTyUnit Nothing]
      
      pure (MLIR.MkExprVariable fresh ty, finalExprs)
    Nothing -> 
      pure (MLIR.MkExprLet name ty expr' MLIR.MkTyUnit Nothing, exprs1)
convertE (MLIR.MkExprCall fn args ty) = do
  (fn', exprs1) <- convertE fn
  (args', exprs2) <- mapAndUnzipM convertE args

  let finalExprs = exprs1 <> concat exprs2
  
  pure (MLIR.MkExprCall fn' args' ty, finalExprs)
convertE (MLIR.MkExprIf cond then_ else_) = do
  (cond', exprs1) <- convertE cond
  (then_', exprs2) <- convertE then_
  (else_', exprs3) <- convertE else_

  let finalExprs = exprs1 <> exprs2 <> exprs3

  pure (MLIR.MkExprIf cond' then_' else_', finalExprs)
convertE (MLIR.MkExprBlock exprs ty) = do
  res <- mapM convertE exprs
  res' <- buildExprs res
  
  pure (MLIR.MkExprBlock res' ty, [])
convertE (MLIR.MkExprField expr field ty) = do
  (expr', exprs) <- convertE expr

  pure (MLIR.MkExprField expr' field ty, exprs)
convertE (MLIR.MkExprTupleAccess expr idx ty) = do
  (expr', exprs) <- convertE expr

  pure (MLIR.MkExprTupleAccess expr' idx ty, exprs)
convertE (MLIR.MkExprLambda args ty expr) = do
  (expr', exprs) <- convertE expr

  pure (MLIR.MkExprLambda args ty expr', exprs)
convertE (MLIR.MkExprPack annots (expr, envs) ty) = do
  (expr', exprs) <- convertE expr
  res <- traverse (traverse convertE) envs

  let (annots', allExprs) = unzip $ map (\(MLIR.MkAnnotation name (envE, exprs')) -> (MLIR.MkAnnotation name envE, exprs')) res

  let finalExprs = exprs <> concat allExprs

  pure (MLIR.MkExprPack annots (expr', annots') ty, finalExprs)
convertE (MLIR.MkExprClosureCall fn args ty) = do
  (fn', exprs1) <- convertE fn
  (args', exprs2) <- mapAndUnzipM convertE args

  let finalExprs = exprs1 <> concat exprs2

  pure (MLIR.MkExprClosureCall fn' args' ty, finalExprs)
convertE (MLIR.MkExprReturn expr) = do
  (expr', exprs) <- convertE expr

  pure (MLIR.MkExprReturn expr', exprs)
convertE (MLIR.MkExprLocated loc expr) = do
  (expr', exprs) <- convertE expr

  pure (MLIR.MkExprLocated loc expr', exprs)

convert :: M.MonadANF m => MLIR.MLIR "declaration" -> m [MLIR.MLIR "declaration"]
convert (MLIR.MkDeclFunction name g args ret body) = do
  (body', stmts) <- convertE body
  let stmts' = map buildDeclFromExpr stmts

  pure (stmts' <> [MLIR.MkDeclFunction name g args ret body'])
convert (MLIR.MkDeclVariable name qvs expr) = do
  (expr', stmts) <- convertE expr
  let stmts' = map buildDeclFromExpr stmts

  pure (stmts' <> [MLIR.MkDeclVariable name qvs expr'])
convert (MLIR.MkDeclNative name g args ret) = pure [MLIR.MkDeclNative name g args ret]
convert (MLIR.MkDeclExtend {}) = compilerError "TODO"

buildDeclFromExpr :: MLIR.MLIR "expression" -> MLIR.MLIR "declaration"
buildDeclFromExpr (MLIR.MkExprLet name ty expr _ _) = do
  let ann = MLIR.MkAnnotation name ty
  MLIR.MkDeclVariable ann [] expr
buildDeclFromExpr e = compilerError $ "Cannot build declaration from expression: " <> show e
