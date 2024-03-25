{-# LANGUAGE LambdaCase #-}

module Plume.Compiler.Desugaring.Desugar where

import Data.IntMap qualified as M
import Data.Set qualified as Set
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
  c@Pre.UEConditionBranch {} -> desugarANF desugarExpr c
  Pre.UETypeOf x -> do
    (x', stmts) <- desugarExpr x
    return (Post.DETypeOf x', stmts)
  d@(Pre.UEDeclaration {}) -> desugarANF desugarExpr d
  s@(Pre.UESwitch {}) -> desugarSwitch (desugarExpr, desugarStatement) s
  Pre.UEBlock xs -> do
    res <- concat <$> mapM desugarStatement xs
    return (Post.DEVar "nil", createBlock res)

desugarStatement
  :: Desugar Pre.UntypedStatement [ANFResult (Maybe Post.DesugaredStatement)]
desugarStatement = \case
  Pre.USExpr x -> do
    (x', stmts) <- desugarExpr x
    case x' of
      Post.DEVar "nil" -> return [(Nothing, stmts)]
      _ -> return [(Just $ Post.DSExpr x', stmts)]
  Pre.USReturn x -> do
    (x', stmts) <- desugarExpr x
    return [(Just $ Post.DSReturn x', stmts)]
  Pre.USDeclaration n e -> do
    (e', stmts) <- desugarExpr e
    return [(Just $ Post.DSDeclaration n e', stmts)]
  Pre.USConditionBranch x y z -> do
    (x', stmts1) <- desugarExpr x
    ys <- desugarStatement y
    zs <- desugarStatement z
    return
      [(Just $ Post.DSConditionBranch x' (createBlock ys) (createBlock zs), stmts1)]

desugarProgram :: Pre.UntypedProgram -> IO [Post.DesugaredProgram]
desugarProgram = \case
  Pre.UPFunction x xs y -> do
    ys <- desugarStatement y
    return [Post.DPFunction x xs (createBlock ys)]
  Pre.UPStatement x -> do
    x' <- desugarStatement x
    return $ map Post.DPStatement $ createBlock x'
  Pre.UPNativeFunction x y -> do
    modifyIORef' nativeFunctions $ Set.insert x
    return [Post.DPNativeFunction x y]

desugar :: [Pre.UntypedProgram] -> IO [Post.DesugaredProgram]
desugar = concatMapM desugarProgram
