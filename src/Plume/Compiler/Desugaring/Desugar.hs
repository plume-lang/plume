{-# LANGUAGE LambdaCase #-}

module Plume.Compiler.Desugaring.Desugar where

import Data.IntMap qualified as M
import Data.Set qualified as Set
import Plume.Compiler.ClosureConversion.Syntax qualified as Pre
import Plume.Compiler.Desugaring.Monad
import Plume.Compiler.Desugaring.Syntax qualified as Post
import Plume.Syntax.Common.Literal
import Plume.Syntax.Translation.Generics

import Plume.Compiler.Desugaring.Modules.ANF
import Plume.Compiler.Desugaring.Modules.Switch

desugarExpr
  :: (IsToplevel, IsReturned, IsExpression)
  -> Desugar Pre.ClosedExpr (ANFResult Post.DesugaredExpr)
desugarExpr isTop = \case
  Pre.CESpecial -> return' Post.DESpecial
  Pre.CEVar x -> return' $ Post.DEVar x
  x@(Pre.CEApplication _ _) -> desugarANF isTop (desugarExpr isTop) x
  Pre.CELiteral x -> return' $ Post.DELiteral x
  Pre.CEList xs -> do
    (xs', stmts) <- mapAndUnzipM (desugarExpr isTop) xs
    return (Post.DEList xs', concat stmts)
  Pre.CEProperty x i -> do
    (x', stmts) <- desugarExpr isTop x
    return (Post.DEProperty x' i, stmts)
  Pre.CEDictionary xs -> do
    (dicts, stmts) <-
      mapAndUnzipM
        ( \(i, x) -> do
            (x', stmts) <- desugarExpr isTop x
            return (M.singleton i x', stmts)
        )
        (M.toList xs)
    let m = mconcat dicts
    return (Post.DEDictionary m, concat stmts)
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

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

desugarProgram :: Pre.ClosedProgram -> IO [Post.DesugaredProgram]
desugarProgram = \case
  Pre.CPFunction x xs y -> do
    ys <- desugarStatement (True, False, False) y
    return [Post.DPFunction x xs (createBlock ys)]
  Pre.CPStatement x -> do
    x' <- desugarStatement (False, False, False) x
    return $ createBlockProg' x'
  Pre.CPNativeFunction fp x y -> do
    modifyIORef' nativeFunctions $ Set.insert x
    return [Post.DPNativeFunction fp x y]
  Pre.CPDeclaration n e -> do
    (e', stmts) <- desugarExpr (False, False, True) e
    return (createBlockProg stmts ++ [Post.DPDeclaration n e'])
  Pre.CPMutDeclaration n e -> do
    (e', stmts) <- desugarExpr (False, False, True) e
    return (createBlockProg stmts ++ [Post.DPMutDeclaration n e'])
  Pre.CPMutUpdate n e -> do
    (e', stmts) <- desugarExpr (False, False, True) e
    return (createBlockProg stmts ++ [Post.DPMutUpdate n e'])

desugar :: [Pre.ClosedProgram] -> IO [Post.DesugaredProgram]
desugar = concatMapM desugarProgram
