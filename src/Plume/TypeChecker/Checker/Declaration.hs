{-# LANGUAGE LambdaCase #-}

module Plume.TypeChecker.Checker.Declaration where

import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Type qualified as Pre
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Constraints.Solver
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post

synthDecl :: Infer -> Infer
synthDecl
  infer
  (Pre.EDeclaration generics (Annotation name ty) expr body) = do
    convertedGenerics :: [TyVar] <- mapM convert generics
    convertedTy :: PlumeType <- convert ty

    searchEnv @"typeEnv" name >>= \case
      Just _ -> throw $ CompilerError "Variable already declared"
      _ -> pure ()

    let scheme = Forall convertedGenerics convertedTy
    insertEnv @"typeEnv" name scheme
    ((exprTy, expr'), s1) <- case convertedGenerics of
      [] -> do
        r@(exprTy, _) <- extractFromArray (local id $ infer expr)
        exprTy `unifiesTo` convertedTy
        pure (r, mempty)
      _ -> do
        (r@(exprTy, _), cs') <-
          local id $ getLocalConstraints $ extractFromArray $ infer expr
        c1 <- createConstraint (exprTy :~: convertedTy)
        let cs'' = cs'.tyConstraints <> [c1]
        writeIORef cyclicCounter 0
        let constraint = MkConstraints cs'' cs'.extConstraints cs'.substitution
        s1 <- solveConstraints constraint
        pure (r, s1)

    let newScheme = Forall (apply s1 convertedGenerics) (apply s1 exprTy)
    insertEnv @"typeEnv" name newScheme

    mapM_
      ( \case
          Pre.GVar v -> deleteEnv @"genericsEnv" v
          _ -> pure ()
      )
      generics

    b <- maybeM body (local id . extractFromArray . infer)
    let retTy = maybe TUnit fst b
    let body' = snd <$> b

    pure (retTy, [Post.EDeclaration (Annotation name exprTy) expr' body'])
synthDecl _ _ = throw $ CompilerError "Only declarations are supported"
