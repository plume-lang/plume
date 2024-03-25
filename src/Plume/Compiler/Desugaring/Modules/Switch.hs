{-# LANGUAGE LambdaCase #-}

module Plume.Compiler.Desugaring.Modules.Switch where

import Data.Map qualified as M
import Plume.Compiler.ClosureConversion.Free
import Plume.Compiler.Desugaring.Monad
import Plume.Compiler.Desugaring.Syntax qualified as Post
import Plume.Compiler.TypeErasure.Syntax qualified as Pre
import Plume.Syntax.Common.Literal

type Desugar' =
  Desugar Pre.UntypedStatement [ANFResult (Maybe Post.DesugaredStatement)]

type Desugar'' = Desugar Pre.UntypedExpr (ANFResult Post.DesugaredExpr)
type DesugarSwitch =
  (Desugar'', Desugar') -> Desugar''

desugarSwitch :: DesugarSwitch
desugarSwitch (fExpr, _) (Pre.UESwitch x cases) = do
  (x', stmts) <- fExpr x
  let (conds, maps) = unzip $ map (createCondition x' . fst) cases
  let dict = mconcat maps
  let lets = stmts <> createLets dict

  let bodies = map snd cases
  let cases' = zip [0 ..] bodies

  res <-
    mapM
      ( \case
          (i, expr) -> do
            let pat = maybeAt i conds
            (expr', stmts'') <- fExpr expr
            let expr'' = substituteMany (M.toList dict) expr'
            let stmts''' = substituteMany (M.toList dict) (stmts'' <> [Post.DSReturn expr''])
            case pat of
              Just conds_ -> do
                let cond = createConditionExpr conds_
                return [Post.DSConditionBranch cond stmts''' []]
              Nothing ->
                return stmts'''
      )
      cases'
  let ifs' = createIfsStatement $ concat res

  return (Post.DEVar "nil", lets <> ifs')
desugarSwitch _ _ = error "test"

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
createIfsStatement _ = error "test"

createIfs
  :: [([Post.DesugaredExpr], Post.DesugaredExpr)] -> Post.DesugaredExpr
createIfs [x] = snd x
createIfs ((cond, body) : xs) =
  if cond' == Post.DELiteral (LBool True)
    then body
    else Post.DEIf cond' body (createIfs xs)
 where
  cond' = createConditionExpr cond
createIfs [] = error "test"

createLets :: Map Text Post.DesugaredExpr -> [Post.DesugaredStatement]
createLets = M.foldrWithKey (\k v acc -> Post.DSDeclaration k v : acc) []

createCondition
  :: Post.DesugaredExpr
  -> Pre.UntypedPattern
  -> ([Post.DesugaredExpr], Map Text Post.DesugaredExpr)
createCondition _ Pre.UPWildcard = ([], mempty)
createCondition x (Pre.UPVariable y) = ([], M.singleton y x)
createCondition x (Pre.UPConstructor y xs) =
  ([Post.DEIsConstructor x y] <> conds, mconcat maps)
 where
  (conds, maps) = zipWithM (createCondition . Post.DEProperty x) [0 ..] xs
createCondition x (Pre.UPLiteral l) =
  ([Post.DEEqualsTo x (Post.DELiteral l)], mempty)