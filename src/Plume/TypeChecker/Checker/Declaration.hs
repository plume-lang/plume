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
  (Pre.EDeclaration isMut generics (Annotation name ty) expr body) = do
    convertedGenerics :: [TyVar] <- mapM convert generics
    convertedTy :: PlumeType <- convert ty

    -- Mutable variables should not have generics as this is unsound
    when (isMut && not (null convertedGenerics)) $ do
      throw $ CompilerError "Generic mutable variables are not supported"

    -- Check if the variable is already declared, if it is and it is mutable
    -- then it should be an update and the update type should have the same 
    -- type as the variable
    searchEnv @"typeEnv" name >>= \case
      Just (Forall _ (TMut t)) -> convertedTy `unifiesTo` t
      Just _ -> throw $ CompilerError "Variable already declared"
      _ -> pure ()

    -- Creating an utility function that shortens the code and a boolean
    -- that indicates if the variable is mutable
    (declFun, isMut') <-
      searchEnv @"typeEnv" name >>= \case
        Just (Forall _ (TMut _)) -> return (Post.EMutUpdate, True)
        Nothing ->
          return $
            if isMut
              then (Post.EMutDeclaration, isMut)
              else (Post.EDeclaration, isMut)
        _ -> throw $ CompilerError "Variable already declared"

    -- Creating the type of the variable based on mutability and adding it
    -- to the environment
    let convertedTy' = if isMut' then TMut convertedTy else convertedTy
    let scheme = Forall convertedGenerics convertedTy'
    insertEnv @"typeEnv" name scheme

    ((exprTy, expr'), s1) <- case convertedGenerics of
      -- If the variable is not generic, we can just infer the expression
      [] -> do
        (exprTy, b) <- extractFromArray (local id $ infer expr)
        let exprTy' = if isMut' then TMut exprTy else exprTy
        exprTy' `unifiesTo` convertedTy'
        pure ((exprTy', b), mempty)
      
      -- If the variable is generic but not mutable, we can infer
      _ | not isMut -> do
        -- Fetching local constraints from inference of the expression
        ((exprTy, b), cs') <-
          local id $ getLocalConstraints $ extractFromArray $ infer expr

        -- Solving the constraints to get rid of the generic types
        c1 <- createConstraint (exprTy :~: convertedTy')
        let cs'' = cs'.tyConstraints <> [c1]
        writeIORef cyclicCounter 0
        let constraint = MkConstraints cs'' cs'.extConstraints cs'.substitution
        s1 <- solveConstraints constraint
        pure ((exprTy, b), s1)

      _ -> throw $ CompilerError "Generic mutable variables are not supported"

    -- Creating a new scheme for the variable based on the substitution
    let newScheme = Forall (apply s1 convertedGenerics) (apply s1 exprTy)
    insertEnv @"typeEnv" name newScheme

    -- Removing the generic types from the environment
    mapM_
      ( \case
          Pre.GVar v -> deleteEnv @"genericsEnv" v
          _ -> pure ()
      )
      generics

    -- Infer the body of the declaration
    b <- maybeM body (local id . extractFromArray . infer)
    let retTy = maybe TUnit fst b
    let body' = snd <$> b

    pure (retTy, [declFun (Annotation name exprTy) expr' body'])
synthDecl _ _ = throw $ CompilerError "Only declarations are supported"
