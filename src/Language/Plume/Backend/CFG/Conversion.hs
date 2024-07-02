module Language.Plume.Backend.CFG.Conversion where
import qualified Language.Plume.Syntax.LLIR as LLIR

doesContainReturn :: LLIR.LLIR "expression" -> Bool
doesContainReturn (LLIR.MkExprReturn _) = True
doesContainReturn (LLIR.MkExprBlock exprs) = any doesContainReturn exprs
doesContainReturn (LLIR.MkExprIf _ then_ else_ _) = doesContainReturn then_ || doesContainReturn else_
doesContainReturn _ = False

type IsExpression = Bool

convertCFG :: LLIR.LLIR "expression" -> IsExpression -> LLIR.LLIR "expression"
convertCFG (LLIR.MkExprReturn expr) b = if b then convertCFG expr True else LLIR.MkExprReturn expr
convertCFG (LLIR.MkExprBlock exprs) _ = LLIR.MkExprBlock $ map (`convertCFG` False) exprs
convertCFG (LLIR.MkExprIf cond then_ else_ t) b = do
  let then' = convertCFG then_ b
  let else' = convertCFG else_ b

  if b then
    LLIR.MkExprTernary cond then' else' t
  else 
    LLIR.MkExprIf cond then' else' t
convertCFG (LLIR.MkExprTernary cond then_ else_ t) b = do
  let then' = convertCFG then_ b
  let else' = convertCFG else_ b

  if b then
    LLIR.MkExprTernary cond then' else' t
  else 
    LLIR.MkExprIf cond then' else' t
convertCFG (LLIR.MkExprLet name ty expr next) b = do
  let expr' = convertCFG expr True
  let next' = traverse convertCFG next b

  LLIR.MkExprLet name ty expr' next'
convertCFG (LLIR.MkExprCall fn args ty) _ = do
  let fn' = convertCFG fn True
  let args' = map (`convertCFG` True) args

  LLIR.MkExprCall fn' args' ty
convertCFG (LLIR.MkExprField expr field ty st) _ = do
  let expr' = convertCFG expr True

  LLIR.MkExprField expr' field ty st
convertCFG (LLIR.MkExprVariable name t) _ = LLIR.MkExprVariable name t
convertCFG (LLIR.MkExprLiteral lit) _ = LLIR.MkExprLiteral lit
convertCFG (LLIR.MkExprStruct name annots) _ = LLIR.MkExprStruct name annots
convertCFG (LLIR.MkExprCast e t) _ = LLIR.MkExprCast (convertCFG e True) t
convertCFG (LLIR.MkExprPtrField e name t) _ = LLIR.MkExprPtrField (convertCFG e True) name t
convertCFG (LLIR.MkExprRef e t) _ = LLIR.MkExprRef (convertCFG e True) t

convert :: LLIR.LLIR "declaration" -> LLIR.LLIR "declaration"
convert (LLIR.MkDeclFunction name gens args t body) = LLIR.MkDeclFunction name gens args t (convertCFG body True)
convert (LLIR.MkDeclNative name gens args t) = LLIR.MkDeclNative name gens args t
convert (LLIR.MkDeclStruct name annots) = LLIR.MkDeclStruct name annots