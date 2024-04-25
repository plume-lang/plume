{-# LANGUAGE LambdaCase #-}

module Plume.TypeChecker.Checker.Declaration where

import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Solver
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post
import Plume.Syntax.Common.Type (getGenericName)
import Plume.TypeChecker.Constraints.Unification (compressPaths)

synthDecl :: Infer -> Infer
synthDecl
  infer
  (Pre.EDeclaration isMut generics (Annotation name ty) expr body) = do
    convertedGenerics :: [PlumeType] <- mapM convert generics
    convertedTy :: PlumeType <- convert ty

    -- Mutable variables should not have generics as this is unsound
    when (isMut && not (null convertedGenerics)) $ do
      throw $ CompilerError "Generic mutable variables are not supported"

    -- Check if the variable is already declared, if it is and it is mutable
    -- then it should be an update and the update type should have the same 
    -- type as the variable
    searchEnv @"typeEnv" name >>= \case
      Just (TMut t) -> convertedTy `unifiesWith` t
      Just _ -> throw $ CompilerError "Variable already declared"
      _ -> pure ()

    -- Creating an utility function that shortens the code and a boolean
    -- that indicates if the variable is mutable
    (declFun, isMut') <-
      searchEnv @"typeEnv" name >>= \case
        Just (TMut t) -> do
          t `unifiesWith` convertedTy
          return (Post.EMutUpdate, True)
        Nothing ->
          return $
            if isMut
              then (Post.EMutDeclaration, isMut)
              else (Post.EDeclaration, isMut)
        _ -> throw $ CompilerError "Variable already declared"

    let mut = if isMut' then TMut else id

    -- Creating the type of the variable based on mutability and adding it
    -- to the environment
    let convertedTy' = mut convertedTy
    let scheme = convertedTy'
    insertEnv @"typeEnv" name scheme

    enterLevel
    (exprTy, expr') <- case convertedGenerics of
      -- If the variable is not generic, we can just infer the expression
      [] -> extractFromArray (local id $ infer expr)
        -- let exprTy' = mut exprTy
        -- exprTy' `unifiesTo` convertedTy'
        -- pure (exprTy', b)
      
      -- If the variable is generic but not mutable, we can infer
      _ | not isMut -> extractFromArray $ infer expr

      _ -> throw $ CompilerError "Generic mutable variables are not supported"

    let exprTy' = mut exprTy
    exprTy' `unifiesWith` convertedTy'

    exitLevel

    -- Creating a new scheme for the variable based on the substitution
    newScheme <- liftIO $ compressPaths exprTy'
    insertEnv @"typeEnv" name newScheme

    when (Post.containsReturn expr') $ throw (DeclarationReturn "declaration")

    -- Removing the generic types from the environment
    mapM_ (deleteEnv @"genericsEnv" . getGenericName) generics

    -- Infer the body of the declaration
    b <- maybeM body (local id . extractFromArray . infer)
    let retTy = maybe TUnit fst b
    let body' = snd <$> b

    pure (retTy, [declFun (Annotation name exprTy) expr' body'])
synthDecl _ _ = throw $ CompilerError "Only declarations are supported"
