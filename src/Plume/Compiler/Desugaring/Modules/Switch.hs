{-# LANGUAGE LambdaCase #-}

module Plume.Compiler.Desugaring.Modules.Switch where

import Data.Map qualified as M
import Plume.Compiler.ClosureConversion.Free
import Plume.Compiler.ClosureConversion.Syntax qualified as Pre
import Plume.Compiler.Desugaring.Monad
import Plume.Compiler.Desugaring.Syntax qualified as Post
import Plume.Syntax.Common.Literal

type Desugar' =
  Desugar Pre.ClosedStatement [ANFResult (Maybe Post.DesugaredStatement)]

type Desugar'' = Desugar Pre.ClosedExpr (ANFResult Post.DesugaredExpr)
type DesugarSwitch =
  (Desugar'', Desugar') -> Desugar''

type IsToplevel = Bool

desugarSwitch :: IsToplevel -> DesugarSwitch
desugarSwitch isTop (fExpr, _) (Pre.CESwitch x cases) = do
  (x', stmts) <- fExpr x
  let (conds, maps) = unzip $ map (createCondition x' . fst) cases

  let bodies = map snd cases
  let cases' = zip3 [0 ..] bodies maps

  res <-
    mapM
      ( \case
          (i, expr, m) -> do
            let pat = maybeAt i conds
            (expr', stmts'') <- fExpr expr
            let lastStmt = if isTop then Post.DSReturn expr' else Post.DSExpr expr'
            let stmts''' = substituteMany (M.toList m) (stmts'' <> [lastStmt])
            case pat of
              Just conds_ -> do
                let cond = createConditionExpr conds_
                return [Post.DSConditionBranch cond stmts''' []]
              Nothing ->
                return stmts'''
      )
      cases'
  let ifs' = createIfsStatement $ concat res

  return (Post.DEVar "nil", stmts <> ifs')
desugarSwitch _ _ _ = error "Received incorrect expression, not a switch."

createConditionExpr :: [Post.DesugaredExpr] -> Post.DesugaredExpr
createConditionExpr [] = Post.DELiteral (LBool True)
createConditionExpr [x] = x
createConditionExpr (x : xs) = Post.DEAnd x (createConditionExpr xs)

createIfsStatement
  :: [Post.DesugaredStatement]
  -> [Post.DesugaredStatement]
createIfsStatement [] = []
createIfsStatement (Post.DSConditionBranch c t [] : xs)
  | c == Post.DELiteral (LBool True) = t
  | otherwise = [Post.DSConditionBranch c t (createIfsStatement xs)]
createIfsStatement _ = []

createIfs
  :: [([Post.DesugaredExpr], Post.DesugaredExpr)] -> Post.DesugaredExpr
createIfs [x] = snd x
createIfs ((cond, body) : xs) =
  if cond' == Post.DELiteral (LBool True)
    then body
    else Post.DEIf cond' body (createIfs xs)
 where
  cond' = createConditionExpr cond
createIfs [] = error "Switch should have at least one case."

createLets :: Map Text Post.DesugaredExpr -> [Post.DesugaredStatement]
createLets = M.foldrWithKey (\k v acc -> Post.DSDeclaration k v : acc) []

createCondition
  :: Post.DesugaredExpr
  -> Pre.ClosedPattern
  -> ([Post.DesugaredExpr], Map Text Post.DesugaredExpr)
createCondition _ Pre.CPWildcard = ([], mempty)
createCondition x (Pre.CPVariable y) = ([], M.singleton y x)
createCondition x (Pre.CPConstructor y xs) =
  let spc = Post.DEEqualsTo (Post.DEProperty x 0) Post.DESpecial
      cons = Post.DEEqualsTo (Post.DEProperty x 2) (Post.DELiteral (LString y))
      (conds, maps) = unzip $ zipWith (createCondition . Post.DEProperty x) [3 ..] xs
   in (spc : cons : concat conds, mconcat maps)
createCondition x (Pre.CPLiteral l) =
  ([Post.DEEqualsTo x (Post.DELiteral l)], mempty)
createCondition x (Pre.CPSpecialVar n) = do
  let spc = Post.DEEqualsTo (Post.DEProperty x 0) Post.DESpecial
  let cons = Post.DEEqualsTo (Post.DEProperty x 2) (Post.DELiteral (LString n))
  ([spc, cons], mempty)
createCondition x (Pre.CPList pats slice) =
  let (conds, maps) =
        unzip $
          zipWith
            createCondition
            [Post.DEProperty x i | i <- [0 .. length pats - 1]]
            pats
      (conds', maps') = maybe mempty (createCondition (Post.DESlice x (length pats))) slice
      patLen = fromIntegral $ length pats
      lenCond = case slice of
        Just _ ->
          Post.DEGreaterThan
            (Post.DEListLength x)
            (Post.DELiteral (LInt (patLen - 1)))
        Nothing ->
          Post.DEEqualsTo
            (Post.DEListLength x)
            (Post.DELiteral (LInt patLen))
   in (lenCond : concat conds <> conds', mconcat maps <> maps')