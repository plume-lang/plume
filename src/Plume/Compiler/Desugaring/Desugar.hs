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

desugarExpr :: Desugar Pre.ClosedExpr (ANFResult Post.DesugaredExpr)
desugarExpr = \case
  Pre.CEVar x -> return' $ Post.DEVar x
  x@(Pre.CEApplication _ _) -> desugarANF desugarExpr x
  Pre.CELiteral x -> return' $ Post.DELiteral x
  Pre.CEList xs -> do
    (xs', stmts) <- mapAndUnzipM desugarExpr xs
    return (Post.DEList xs', concat stmts)
  Pre.CEProperty x i -> do
    (x', stmts) <- desugarExpr x
    return (Post.DEProperty x' i, stmts)
  Pre.CEDictionary xs -> do
    (dicts, stmts) <-
      mapAndUnzipM
        ( \(i, x) -> do
            (x', stmts) <- desugarExpr x
            return (M.singleton i x', stmts)
        )
        (M.toList xs)
    let m = mconcat dicts
    return (Post.DEDictionary m, concat stmts)
  c@Pre.CEConditionBranch {} -> desugarANF desugarExpr c
  Pre.CEEqualsType x t -> do
    (x', stmts) <- desugarExpr x
    return (Post.DEEqualsTo (Post.DETypeOf x') (Post.DELiteral (LString t)), stmts)
  d@(Pre.CEDeclaration {}) -> desugarANF desugarExpr d
  s@(Pre.CESwitch {}) -> desugarSwitch (desugarExpr, desugarStatement) s
  Pre.CEBlock xs -> do
    res <- concat <$> mapM desugarStatement xs
    return (Post.DEVar "nil", createBlock res)
  Pre.CEAnd x y -> do
    (x', stmts1) <- desugarExpr x
    (y', stmts2) <- desugarExpr y
    return (Post.DEAnd x' y', stmts1 ++ stmts2)
  Pre.CEIndex x y -> do
    (x', stmts1) <- desugarExpr x
    (y', stmts2) <- desugarExpr y
    return (Post.DEIndex x' y', stmts1 ++ stmts2)

desugarStatement
  :: Desugar Pre.ClosedStatement [ANFResult (Maybe Post.DesugaredStatement)]
desugarStatement = \case
  Pre.CSExpr x -> do
    (x', stmts) <- desugarExpr x
    case x' of
      Post.DEVar "nil" -> return [(Nothing, stmts)]
      _ -> return [(Just $ Post.DSExpr x', stmts)]
  Pre.CSReturn x -> do
    (x', stmts) <- desugarExpr x
    return [(Just $ Post.DSReturn x', stmts)]
  Pre.CSDeclaration n e -> do
    (e', stmts) <- desugarExpr e
    return [(Just $ Post.DSDeclaration n e', stmts)]
  Pre.CSConditionBranch x y z -> do
    (x', stmts1) <- desugarExpr x
    ys <- desugarStatement y
    zs <- desugarStatement z
    return
      [(Just $ Post.DSConditionBranch x' (createBlock ys) (createBlock zs), stmts1)]

desugarProgram :: Pre.ClosedProgram -> IO [Post.DesugaredProgram]
desugarProgram = \case
  Pre.CPFunction x xs y -> do
    ys <- desugarStatement y
    return [Post.DPFunction x xs (createBlock ys)]
  Pre.CPStatement x -> do
    x' <- desugarStatement x
    return $ map Post.DPStatement $ createBlock x'
  Pre.CPNativeFunction x y -> do
    modifyIORef' nativeFunctions $ Set.insert x
    return [Post.DPNativeFunction x y]

desugar :: [Pre.ClosedProgram] -> IO [Post.DesugaredProgram]
desugar = concatMapM desugarProgram
