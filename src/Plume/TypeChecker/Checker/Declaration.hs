{-# LANGUAGE LambdaCase #-}

module Plume.TypeChecker.Checker.Declaration where

import Data.List qualified as List
import Data.Map qualified as Map
import Data.Tuple.Utils (fst3, snd3, thd3)
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Type (getGenericName)
import Plume.Syntax.Translation.Generics (concatMapM)
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Solver
import Plume.TypeChecker.Constraints.Typeclass (discharge, removeDuplicatesAssumps, removeDuplicatesQuals)
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.Checker.Extension
import Plume.TypeChecker.Monad.Free (substituteVar)
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.TLIR qualified as Post
import Prelude hiding (gets, local)

synthDecl :: Infer -> Infer
synthDecl
  infer
  (Pre.EDeclaration isMut generics (Annotation name ty) expr body) = do
    convertedGenerics :: [PlumeQualifier] <- concatMapM convert generics
    convertedTy :: PlumeType <- convert ty

    let qvars = getQVars convertedGenerics

    -- Mutable variables should not have generics as this is unsound
    when (isMut && not (null convertedGenerics)) $ do
      throw $ CompilerError "Generic mutable variables are not supported"

    -- Check if the variable is already declared, if it is and it is mutable
    -- then it should be an update and the update type should have the same
    -- type as the variable
    searchEnv @"typeEnv" name >>= \case
      Just (Forall _ (_ :=>: (TMut t))) -> convertedTy `unifiesWith` t
      Just _ -> throw $ CompilerError "Variable already declared"
      _ -> pure ()

    -- Creating an utility function that shortens the code and a boolean
    -- that indicates if the variable is mutable
    (declFun, isMut') <-
      searchEnv @"typeEnv" name >>= \case
        Just (Forall _ (_ :=>: (TMut t))) -> do
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
    let scheme = Forall qvars $ convertedGenerics :=>: convertedTy'
    insertEnv @"typeEnv" name scheme

    enterLevel
    (exprTy, ps, h) <- case convertedGenerics of
      -- If the variable is not generic, we can just infer the expression
      [] -> local id $ infer expr
      -- If the variable is generic but not mutable, we can infer
      _ | not isMut -> infer expr
      _ -> throw $ CompilerError "Generic mutable variables are not supported"

    let ty' = mut exprTy
    convertedTy' `unifiesWith` ty'

    cenv <- gets (extendEnv . environment)

    res' <- traverse (discharge cenv) ps
    let (_ps, m2, as, _) = List.unzip4 res'
    let ps' = concatMap removeQVars _ps

    ps'' <- removeDuplicatesQuals ps'

    let (_ :=>: t) = List.nub ps'' :=>: ty'

    m' <- liftIO $ mapM (firstM compressQual) $ List.nub $ concat m2

    let names = getMapNames m'
    let as' = keepAssumpWithName (concat as) names

    (sub, as'') <- removeDuplicatesAssumps as'

    let finalM = m'

    let finalExprs = map (second getAllElements) finalM
    let finalExprs' = List.nub $ concatMap (\(_, e) -> map (\case
            Post.EVariable n t' -> (n, t')
            _ -> error "Not a variable"
          ) e) finalExprs

    sub' <- unify finalExprs'

    let sub'' = Map.toList sub' <> sub

    h' <- liftIO $ runReaderT h $ getExpr finalM
    let h'' = List.foldl substituteVar h' sub''

    let args = map (\(n :>: t') -> n :@: t') as''
    let tys' = map (\(_ :>: t') -> t') as''
  
    cTy' <- liftIO $ compressPaths ty'
    
    pos <- fetchPosition

    let clos = if null args then h'' else Post.EClosure args cTy' h'' pos
    let closTy = if null args then t else tys' :->: t

    exitLevel

    -- Creating a new scheme for the variable based on the substitution
    newScheme <- liftIO $ compressPaths t
    insertEnv @"typeEnv" name (Forall qvars $ convertedGenerics :=>: newScheme)

    -- Removing the generic types from the environment
    mapM_ (deleteEnv @"genericsEnv" . getGenericName) generics

    -- Infer the body of the declaration
    b <- maybeM body (local id . infer)
    let retTy = maybe TUnit fst3 b
    let body' = mapM thd3 b
    let psb = maybe [] snd3 b

    pure (retTy, psb, declFun (Annotation name closTy) clos <$> body')
synthDecl _ _ = throw $ CompilerError "Only declarations are supported"
