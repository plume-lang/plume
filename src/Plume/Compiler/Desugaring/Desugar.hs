{-# LANGUAGE LambdaCase #-}

module Plume.Compiler.Desugaring.Desugar where

import Data.Map qualified as M 
import Data.IntMap qualified as IM
import Data.Set qualified as Set
import Data.Tuple.Utils
import Plume.Compiler.ClosureConversion.Syntax qualified as Pre
import Plume.Compiler.Desugaring.Monad
import Plume.Compiler.Desugaring.Syntax qualified as Post
import Plume.Syntax.Common.Literal
import Plume.Syntax.Translation.Generics

import Plume.Compiler.Desugaring.Modules.ANF
import Plume.Compiler.Desugaring.Modules.Switch

-- | DESUGARING
-- | Desugaring is the process of transforming the AST into a simpler form.
-- | This is done to simplify complex AST structures such as pattern matching.
-- | The desugaring process is done in multiple steps, each step transforming
-- | the AST into a simpler form.
-- |
-- | There are some generic steps:
-- |
-- |  - Desugar to ANF (A-normal form): This step transforms the AST into a
-- |    simpler form where every expression is a variable or a literal.
-- |    And there no more complex let-in expressions (let-in expressions are
-- |    transformed into a sequence of statements).
-- |
-- |  - Desugar switch: This step transforms the switch expressions into a
-- |    sequence of if-else expressions.

desugarExpr
  :: (IsToplevel, IsReturned, IsExpression)
  -> Desugar Pre.ClosedExpr (ANFResult Post.DesugaredExpr)
desugarExpr isTop = \case
  Pre.CESpecial -> return' Post.DESpecial
  Pre.CEVar x -> return' $ Post.DEVar x
  Pre.CEApplication (Pre.CEVar ">") [x, Pre.CELiteral (LInt i)] -> do
    (x', stmts) <- desugarExpr isTop x
    return (Post.DEGreaterThan x' (fromInteger i), stmts)
  Pre.CEApplication (Pre.CEVar "@list_length") [x] -> do
    (x', stmts) <- desugarExpr isTop x
    return (Post.DEListLength x', stmts)
  x@(Pre.CEApplication _ _) -> desugarANF isTop (desugarExpr isTop) x
  Pre.CELiteral x -> return' $ Post.DELiteral x
  Pre.CEList xs -> do
    (xs', stmts) <- mapAndUnzipM (desugarExpr isTop) xs
    return (Post.DEList xs', concat stmts)
  Pre.CEEqualsTo x y -> do
    (x', stmts1) <- desugarExpr isTop x
    (y', stmts2) <- desugarExpr isTop y
    return (Post.DEEqualsTo x' y', stmts1 ++ stmts2)
  Pre.CEProperty x i -> do
    (x', stmts) <- desugarExpr isTop x
    case readEither (toString i) of
      Right i' -> return (Post.DEProperty x' i', stmts)
      Left _ -> error $ "Invalid property index, received: " <> show i
  Pre.CEDictionary xs -> do
    (dicts, stmts) <-
      mapAndUnzipM
        ( \(idxStr, x) -> do
            case readEither (toString idxStr) of
              Right idx -> do
                (x', stmts) <- desugarExpr isTop x
                return (M.singleton (idx :: Int) x', stmts)
              Left _ -> error $ "Invalid dictionary index, got: " <> show idxStr
        )
        (M.toList xs)
    let m = mconcat dicts
    let m' = IM.fromList $ zip [0 ..] (M.elems m)
    return (Post.DEDictionary m', concat stmts)
  c@Pre.CEConditionBranch {} -> desugarANF isTop (desugarExpr isTop) c
  Pre.CEEqualsType x t -> do
    (x', stmts) <- desugarExpr isTop x
    return (Post.DEEqualsTo (Post.DETypeOf x') (Post.DELiteral (LString t)), stmts)
  d@(Pre.CEDeclaration {}) -> desugarANF isTop (desugarExpr isTop) d
  d@(Pre.CEMutDeclaration {}) -> desugarANF isTop (desugarExpr isTop) d
  d@(Pre.CEMutUpdate {}) -> desugarANF isTop (desugarExpr isTop) d
  s@Pre.CESwitch {} -> desugarSwitch isTop (desugarExpr isTop, desugarStatement isTop) s
  Pre.CEBlock xs -> do
    res <- concat <$> mapM (desugarStatement (fst3 isTop, False, False)) xs
    return (Post.DEVar "nil", createBlock res)
  Pre.CEAnd x y -> do
    (x', stmts1) <- desugarExpr isTop x
    (y', stmts2) <- desugarExpr isTop y
    return (Post.DEAnd x' y', stmts1 ++ stmts2)
  Pre.CEIndex x y -> do
    (x', stmts1) <- desugarExpr isTop x
    (y', stmts2) <- desugarExpr isTop y
    return (Post.DEIndex x' y', stmts1 ++ stmts2)
  Pre.CEUnMut x -> do
    (x', stmts) <- desugarExpr isTop x
    return (Post.DEUnMut x', stmts)
  Pre.CESlice x i -> do
    (x', stmts) <- desugarExpr isTop x
    return (Post.DESlice x' i, stmts)
    
desugarStatement
  :: (IsToplevel, IsReturned, IsExpression)
  -> Desugar Pre.ClosedStatement [ANFResult (Maybe Post.DesugaredStatement)]
desugarStatement isTop = \case
  Pre.CSExpr x -> do
    (x', stmts) <- desugarExpr isTop x
    case x' of
      Post.DEVar "nil" -> return [(Nothing, stmts)]
      _ -> return [(Just $ Post.DSExpr x', stmts)]
  Pre.CSReturn x -> do
    (x', stmts) <- desugarExpr (False, True, True) x
    return [(Just $ Post.DSReturn x', stmts)]
  Pre.CSDeclaration n e -> do
    (e', stmts) <- desugarExpr (False, False, True) e
    return [(Just $ Post.DSDeclaration n e', stmts)]
  Pre.CSConditionBranch x y z -> do
    (x', stmts1) <- desugarExpr (False, snd3 isTop, False) x
    ys <- desugarStatement isTop y
    zs <- desugarStatement isTop z
    return
      [(Just . Post.DSExpr $ Post.DEIf x' (createBlock ys) (createBlock zs), stmts1)]
  Pre.CSMutDeclaration n e -> do
    (e', stmts) <- desugarExpr (False, False, True) e
    return [(Just $ Post.DSMutDeclaration n e', stmts)]
  Pre.CSMutUpdate n e -> do
    (e', stmts) <- desugarExpr (False, False, True) e
    return [(Just $ Post.DSMutUpdate n e', stmts)]

desugarProgram :: Pre.ClosedProgram -> IO [Post.DesugaredProgram]
desugarProgram = \case
  Pre.CPFunction x xs y isAsync -> do
    ys <- desugarStatement (True, False, False) y
    return [Post.DPFunction x xs (createBlock ys) isAsync]
  Pre.CPStatement x -> do
    x' <- desugarStatement (False, False, False) x
    return $ createBlockProg' x'
  Pre.CPNativeFunction fp x y st -> do
    modifyIORef' nativeFunctions $ Set.insert x
    return [Post.DPNativeFunction fp x y st]
  Pre.CPDeclaration n e -> do
    (e', stmts) <- desugarExpr (False, False, True) e
    return (createBlockProg stmts ++ [Post.DPDeclaration n e'])
  Pre.CPMutDeclaration n e -> do
    (e', stmts) <- desugarExpr (False, False, True) e
    return (createBlockProg stmts ++ [Post.DPMutDeclaration n e'])
  Pre.CPMutUpdate n e -> do
    (e', stmts) <- desugarExpr (False, False, True) e
    return (createBlockProg stmts ++ [Post.DPMutUpdate n e'])
  Pre.CPDeclare n -> return [Post.DPDeclare n]

desugar :: [Pre.ClosedProgram] -> IO [Post.DesugaredProgram]
desugar = concatMapM desugarProgram
