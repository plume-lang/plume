module Language.Plume.Backend.TLIR where

import Language.Plume.Syntax.Internal.Annotation as Ann
import Language.Plume.Syntax.HLIR qualified as HLIR
import Language.Plume.Syntax.MLIR qualified as MLIR

unIdentity :: Identity a -> a
unIdentity (Identity a) = a

declToMLIR :: HLIR.HLIR "declaration" -> MLIR.MLIR "declaration"
declToMLIR (HLIR.MkDeclFunction name gens args (Identity ret) body) = do
  let args' = map (fmap unIdentity) args

  MLIR.MkDeclFunction name gens args' ret (exprToMLIR body)
declToMLIR (HLIR.MkDeclVariable ann qvs expr) =
  MLIR.MkDeclVariable (fmap unIdentity ann) qvs (exprToMLIR expr)
declToMLIR (HLIR.MkDeclLocated _ decl) =
  declToMLIR decl
declToMLIR (HLIR.MkDeclPublic _) = error "public declarations not supported"
declToMLIR (HLIR.MkDeclRequire _) = error "require declarations not supported"
declToMLIR (HLIR.MkDeclNative name gens args ret) =
  MLIR.MkDeclNative name gens args ret

exprToMLIR :: HLIR.HLIR "expression" -> MLIR.MLIR "expression"
exprToMLIR (HLIR.MkExprLiteral lit) = MLIR.MkExprLiteral lit
exprToMLIR (HLIR.MkExprVariable ann) =
  MLIR.MkExprVariable ann.name (unIdentity ann.value)
exprToMLIR (HLIR.MkExprCall fn args (Identity t)) =
  MLIR.MkExprCall (exprToMLIR fn) (map exprToMLIR args) t
exprToMLIR (HLIR.MkExprAnnotation expr _) = exprToMLIR expr
exprToMLIR (HLIR.MkExprLambda args (Identity ret) body) =
  MLIR.MkExprLambda (map (fmap unIdentity) args) ret (exprToMLIR body)
exprToMLIR (HLIR.MkExprIf cond then_ else_) =
  MLIR.MkExprIf (exprToMLIR cond) (exprToMLIR then_) (exprToMLIR else_)
exprToMLIR (HLIR.MkExprBlock exprs t) = MLIR.MkExprBlock (map exprToMLIR exprs) (unIdentity t)
exprToMLIR (HLIR.MkExprLocated _ e) = exprToMLIR e
exprToMLIR (HLIR.MkExprReturn e) = MLIR.MkExprReturn (exprToMLIR e)
