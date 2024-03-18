{-# LANGUAGE LambdaCase #-}

module Plume.Compiler.Desugaring.Desugar where

import Data.IntMap qualified as M
import Plume.Compiler.Desugaring.Monad
import Plume.Compiler.Desugaring.Syntax qualified as Post
import Plume.Compiler.TypeErasure.Syntax qualified as Pre
import Plume.Syntax.Translation.Generics

import Plume.Compiler.Desugaring.Modules.ANF
import Plume.Compiler.Desugaring.Modules.Switch

desugarExpr :: Desugar Pre.UntypedExpr (ANFResult Post.DesugaredExpr)
desugarExpr = \case
  Pre.UEVar x -> return' $ Post.DEVar x
  x@(Pre.UEApplication _ _) -> desugarANF desugarExpr x
  Pre.UELiteral x -> return' $ Post.DELiteral x
  Pre.UEList xs -> do
    (xs', stmts) <- mapAndUnzipM desugarExpr xs
    return (Post.DEList xs', concat stmts)
  Pre.UEProperty x i -> do
    (x', stmts) <- desugarExpr x
    return (Post.DEProperty x' i, stmts)
  Pre.UEDictionary xs -> do
    (dicts, stmts) <-
      mapAndUnzipM
        ( \(i, x) -> do
            (x', stmts) <- desugarExpr x
            return (M.singleton i x', stmts)
        )
        (M.toList xs)
    let m = mconcat dicts
    return (Post.DEDictionary m, concat stmts)
  Pre.UEConditionBranch x y z -> do
    (x', stmts1) <- desugarExpr x
    (y', stmts2) <- desugarExpr y
    (z', stmts3) <- desugarExpr z
    return (Post.DEIf x' y' z', stmts1 <> stmts2 <> stmts3)
  Pre.UETypeOf x -> do
    (x', stmts) <- desugarExpr x
    return (Post.DETypeOf x', stmts)
  d@(Pre.UEDeclaration {}) -> desugarANF desugarExpr d
  s@(Pre.UESwitch {}) -> desugarSwitch (desugarExpr, desugarStatement) s
  Pre.UEBlock xs -> do
    (xs', stmts) <- unzip . concat <$> mapM desugarStatement xs
    return (Post.DEVar "nil", concat stmts <> xs')

desugarStatement
  :: Desugar Pre.UntypedStatement [ANFResult Post.DesugaredStatement]
desugarStatement = \case
  Pre.USExpr x -> do
    (x', stmts) <- desugarExpr x
    return [(Post.DSExpr x', stmts)]
  Pre.USReturn x -> do
    (x', stmts) <- desugarExpr x
    return [(Post.DSReturn x', stmts)]
  Pre.USDeclaration n e -> do
    (e', stmts) <- desugarExpr e
    return [(Post.DSDeclaration n e', stmts)]

desugarProgram :: Pre.UntypedProgram -> IO [Post.DesugaredProgram]
desugarProgram = \case
  Pre.UPFunction x xs y -> do
    ys <- desugarStatement y
    return [Post.DPFunction x xs (createBlock ys)]
  Pre.UPStatement x -> do
    x' <- desugarStatement x
    return $ map Post.DPStatement $ createBlock x'
  Pre.UPNativeFunction x y -> return [Post.DPNativeFunction x y]

desugar :: [Pre.UntypedProgram] -> IO [Post.DesugaredProgram]
desugar = concatMapM desugarProgram
