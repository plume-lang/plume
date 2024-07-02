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
exprToMLIR (HLIR.MkExprSwitch e cases _) = do
  let e' = exprToMLIR e
  let cases' = map (bimap (`createCondition` e') exprToMLIR) cases

  createIfs cases'

createIfs :: [(([MLIR.MLIR "expression"], [MLIR.MLIR "expression"]), MLIR.MLIR "expression")] -> MLIR.MLIR "expression"
createIfs [((_, lets), expr)] = if null lets then expr else MLIR.MkExprBlock (lets <> [expr]) MLIR.MkTyUnit
createIfs (((conds, lets), expr):as) = do
  let conds' = createIfCondition conds
  
  let bl = if null lets then expr else MLIR.MkExprBlock (lets <> [expr]) MLIR.MkTyUnit
  
  MLIR.MkExprIf conds' bl (createIfs as)
createIfs [] = error "empty conditions"

createIfCondition :: [MLIR.MLIR "expression"] -> MLIR.MLIR "expression"
createIfCondition [] = error "empty conditions"
createIfCondition [a] = a
createIfCondition (a:as) = a `and'` createIfCondition as

and' :: MLIR.MLIR "expression" -> MLIR.MLIR "expression" -> MLIR.MLIR "expression"
and' a b = MLIR.MkExprCall (MLIR.MkExprVariable "&&" (binaryTy MLIR.MkTyBool)) [a, b] MLIR.MkTyBool

binaryTy :: MLIR.MLIR "type" -> MLIR.MLIR "type"
binaryTy ty = [ty, ty] MLIR.:->: MLIR.MkTyBool

strcmp :: MLIR.MLIR "type"
strcmp = binaryTy MLIR.MkTyString

createCondition :: HLIR.HLIR "pattern" -> MLIR.MLIR "expression" -> ([MLIR.MLIR "expression"], [MLIR.MLIR "expression"])
createCondition (HLIR.MkPatternLiteral str@(HLIR.MkString _)) e = do
  ([MLIR.MkExprCall (MLIR.MkExprVariable "strcmp" strcmp) [e, MLIR.MkExprLiteral str] strcmp], [])
createCondition (HLIR.MkPatternVariable ann) e = do
  let let' = MLIR.MkExprLet ann.name (unIdentity ann.value) e MLIR.MkTyUnit Nothing

  (mempty, [let'])
createCondition (HLIR.MkPatternLocated _ p) e = createCondition p e
createCondition (HLIR.MkPatternWildcard _) _ = (mempty, mempty)
createCondition (HLIR.MkPatternLiteral l@(MLIR.MkInteger _)) e = do
  let binTy = binaryTy MLIR.MkTyInt
  ([MLIR.MkExprCall (MLIR.MkExprVariable "==" binTy) [e, MLIR.MkExprLiteral l] binTy], [])
createCondition (HLIR.MkPatternLiteral l@(MLIR.MkFloat _)) e = do
  let binTy = binaryTy MLIR.MkTyFloat
  ([MLIR.MkExprCall (MLIR.MkExprVariable "==" binTy) [e, MLIR.MkExprLiteral l] binTy], [])
createCondition (HLIR.MkPatternLiteral l@(MLIR.MkChar _)) e = do
  let binTy = binaryTy MLIR.MkTyChar
  ([MLIR.MkExprCall (MLIR.MkExprVariable "==" binTy) [e, MLIR.MkExprLiteral l] binTy], [])
createCondition _ _ = error "special patterns not supported"