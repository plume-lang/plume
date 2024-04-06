{-# LANGUAGE LambdaCase #-}

module Plume.TypeChecker.Checker where

import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Translation.Generics hiding (Error (..), withPosition)
import Plume.TypeChecker.Checker.Application
import Plume.TypeChecker.Checker.Closure
import Plume.TypeChecker.Checker.Condition
import Plume.TypeChecker.Checker.Datatype
import Plume.TypeChecker.Checker.Declaration
import Plume.TypeChecker.Checker.Extension
import Plume.TypeChecker.Checker.Native
import Plume.TypeChecker.Checker.Switch
import Plume.TypeChecker.Constraints.Solver
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Monad
import Plume.TypeChecker.TLIR qualified as Post

synthesize :: Pre.Expression -> Checker (PlumeType, [Post.Expression])
-- Some basic and primitive expressions
synthesize (Pre.ELocated expr pos) = withPosition pos $ synthesize expr
synthesize (Pre.EVariable name) = do
  searchEnv @"typeEnv" name >>= \case
    Just scheme -> do
      ty <- instantiate scheme
      pure (ty, [Post.EVariable name ty])
    Nothing ->
      searchEnv @"datatypeEnv" name >>= \case
        Just sch -> do
          ty <- instantiate sch
          pure (ty, [Post.EVariable name ty])
        Nothing -> do
          extExists <- doesExtensionExist name
          if extExists
            then do
              ty <- fresh
              futureFunTy <- fresh
              doesExtend ty name futureFunTy
              pure (futureFunTy, [Post.EExtVariable name futureFunTy ty])
            else throw $ UnboundVariable name
synthesize (Pre.ELiteral lit) =
  pure $ (: []) . Post.ELiteral <$> typeOfLiteral lit
synthesize (Pre.EBlock exprs) = local id $ do
  (tys, exprs') <-
    mapAndUnzipM
      (localPosition . extractFromArray . synthesize)
      exprs
  case viaNonEmpty last tys of
    Nothing -> pure (TUnit, [Post.EBlock []])
    Just retInfered -> do
      returnTy <- gets returnType
      forM_ returnTy $ unifiesTo retInfered
      pure (retInfered, [Post.EBlock exprs'])
synthesize (Pre.EReturn expr) = do
  (ty, expr') <- extractFromArray $ synthesize expr
  returnTy <- gets returnType
  forM_ returnTy $ unifiesTo ty
  pure (ty, [Post.EReturn expr'])
synthesize (Pre.EList xs) = do
  tv <- fresh
  (tys, xs') <-
    mapAndUnzipM
      (local id . localPosition . extractFromArray . synthesize)
      xs
  forM_ tys $ unifiesTo tv
  pure (TList tv, [Post.EList xs'])
-- Calling synthesis modules
synthesize app@(Pre.EApplication {}) = synthApp synthesize app
synthesize clos@(Pre.EClosure {}) = synthClosure synthesize clos
synthesize decl@(Pre.EDeclaration {}) = synthDecl synthesize decl
synthesize cond@(Pre.EConditionBranch {}) = synthCond synthesize cond
synthesize ext@(Pre.ETypeExtension {}) = synthExt synthesize ext
synthesize ty@(Pre.EType {}) = synthDataType ty
synthesize sw@(Pre.ESwitch {}) = synthSwitch synthesize sw
synthesize nat@(Pre.ENativeFunction {}) = synthNative nat
-- This should never be called
synthesize e = throw . CompilerError $ "Not implemented: " <> show e

synthesizeMany :: [Pre.Expression] -> Checker [Post.Expression]
synthesizeMany xs = do
  xs' <- concatMapM (fmap snd . localPosition . synthesize) xs
  cs <- gets constraints
  s1 <- solveConstraints cs
  pure (apply s1 xs')

runSynthesize :: [Pre.Expression] -> IO (Either PlumeError [Post.Expression])
runSynthesize = runChecker . synthesizeMany
