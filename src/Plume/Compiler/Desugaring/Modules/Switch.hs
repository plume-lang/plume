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
type IsReturned = Bool
type IsExpression = Bool

desugarSwitch :: (IsToplevel, IsReturned, IsExpression) -> DesugarSwitch
desugarSwitch (_, shouldReturn, isExpr) (fExpr, _) (Pre.CESwitch x cases) = do
  (x', stmts) <- fExpr x
  let (decl, declVar) = case x' of
        Post.DEVar _ -> ([], x')
        _ -> (createLets (M.singleton "$switch" x'), Post.DEVar "$switch")
  let (conds, maps) = unzip $ map (createCondition declVar . fst) cases

  let bodies = map snd cases
  let cases' = zip3 [0 ..] bodies maps

  res <-
    mapM
      ( \case
          (i, expr, m) -> do
            let pat = maybeAt i conds
            (expr', stmts'') <- fExpr expr
            let lastStmt =
                  if shouldReturn
                    then Post.DSReturn expr'
                    else Post.DSExpr expr'
            let stmts''' = substituteMany (M.toList m) (stmts'' <> [lastStmt])
            return (createConditionExpr (fromMaybe [] pat) stmts''')
      )
      cases'

  if isExpr
    then do
      let ifs' = map unboxStatement (concat res)
      let ifs'' = createIfsExpr ifs'
      return (ifs'', stmts <> decl)
    else do
      let ifs' = createIfsStatement $ concat res
      return (Post.DEVar "nil", stmts <> decl <> ifs')
desugarSwitch _ _ _ = error "Received incorrect expression, not a switch."

unboxStatement :: Post.DesugaredStatement -> Post.DesugaredExpr
unboxStatement (Post.DSExpr x) = x
unboxStatement _ = error "Incorrect statement"

createConditionExpr
  :: [Post.DesugaredExpr] -> [Post.DesugaredStatement] -> [Post.DesugaredStatement]
createConditionExpr [] e = [Post.DSExpr (Post.DEIf (Post.DELiteral (LBool True)) e [])]
createConditionExpr (x : xs) e = [Post.DSExpr $ Post.DEIf x (createConditionExpr xs e) []]

createIfsStatement
  :: [Post.DesugaredStatement]
  -> [Post.DesugaredStatement]
createIfsStatement [] = []
createIfsStatement (Post.DSExpr (Post.DEIf c t []) : xs)
  | c == Post.DELiteral (LBool True) = t
  | otherwise = [Post.DSExpr (Post.DEIf c t (createIfsStatement xs))]
createIfsStatement (x : xs) = x : createIfsStatement xs

createIfsExpr
  :: [Post.DesugaredExpr]
  -> Post.DesugaredExpr
createIfsExpr [] = Post.DEVar "nil"
createIfsExpr [x] = x
createIfsExpr (Post.DEIf c t _ : xs) = Post.DEIf c t [e]
 where
  e = Post.DSExpr $ createIfsExpr xs
createIfsExpr _ = error "Incorrect list of conditions"

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