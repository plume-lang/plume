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
  (bodies', stmts') <-
    first (substituteMany (M.toList dict))
      <$> mapAndUnzipM fExpr bodies

  let newMap = zip conds bodies'

  let ifs = createIfs newMap
  return (ifs, lets <> concat stmts')
desugarSwitch _ _ = error "test"

createConditionExpr :: [Post.DesugaredExpr] -> Post.DesugaredExpr
createConditionExpr = foldr Post.DEAnd (Post.DELiteral $ LBool True)

createIfs
  :: [([Post.DesugaredExpr], Post.DesugaredExpr)] -> Post.DesugaredExpr
createIfs [x] = snd x
createIfs ((cond, body) : xs) =
  Post.DEIf (createConditionExpr cond) body (createIfs xs)
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