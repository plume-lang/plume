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
import Plume.TypeChecker.Constraints.Typeclass
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.Checker.Extension
import Plume.TypeChecker.Monad.Free (substituteVar)
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.TLIR qualified as Post
import Prelude hiding (gets, local)

synthDecl :: Bool -> Infer -> Infer
synthDecl
  isToplevel
  infer
  (Pre.EDeclaration generics (Annotation name ty isMut) expr body) = do
    convertedGenerics :: [PlumeQualifier] <- concatMapM convert generics
    convertedTy :: PlumeType <- convert ty

    let qvars = getQVars convertedGenerics

    -- Mutable variables should not have generics as this is unsound
    when (isMut && not (null convertedGenerics)) $ do
      throw $ CompilerError "Generic mutable variables are not supported"

    -- Check if the variable is already declared, if it is and it is mutable
    -- then it should be an update and the update type should have the same
    -- type as the variable
    searchEnv @"typeEnv" name.identifier >>= \case
      Just (Forall _ (_ :=>: (TMut t))) -> convertedTy `unifiesWith` t
      _ -> pure ()

    -- Creating an utility function that shortens the code and a boolean
    -- that indicates if the variable is mutable
    (declFun, isMut') <-
      searchEnv @"typeEnv" name.identifier >>= \case
        Just (Forall _ (_ :=>: (TMut t))) -> do
          t `unifiesWith` convertedTy
          return (Post.EMutUpdate, True)
        Nothing ->
          return (Post.EDeclaration [], isMut)
        Just _ -> return (Post.EDeclaration [], isMut)

    let mut = if isMut' then TMut else id
    -- Creating the type of the variable based on mutability and adding it
    -- to the environment
    let convertedTy' = mut convertedTy

    let scheme = Forall qvars $ convertedGenerics :=>: convertedTy'
    insertEnv @"typeEnv" name.identifier scheme

    enterLevel
    (exprTy, ps, h) <- case convertedGenerics of
      -- If the variable is not generic, we can just infer the expression
      [] -> local id $ infer expr
      -- If the variable is generic but not mutable, we can infer
      _ | not isMut -> infer expr
      _ -> throw $ CompilerError "Generic mutable variables are not supported"

    let ty' = mut exprTy
    convertedTy' `unifiesWith` ty'

    -- If there are no user-quantified variables and if the declaration is not
    -- toplevel, then just return the inferred expression as it was inferred.
    (clos, closTy, remainingPs, sch) <- if null qvars && not isToplevel then do
      -- ps' <- removeDuplicatesQuals ps
      return (h, ty', ps, scheme)
    else do
      cenv <- gets (extendEnv . environment)

      -- Removing common superclasses between user-given extensions and found
      -- extensions in the inferred expression.
      (__ps, scs) <- do
          p2 <- removeSuperclasses ps convertedGenerics
          return (List.nub p2, List.nub convertedGenerics)

      -- Getting all the needed qualifiers to qualify further the expression
      res' <- traverse (discharge cenv) ps
      let (_ps, m2, as, _) = List.unzip4 res'
      let ps' = concatMap removeQVars _ps

      _ps'' <- mapM (liftIO . compressQual) ps'
      ps'' <- liftIO . mapM compressQual =<< removeSameQualifiers qvars _ps''

      let ty''@(_ :=>: t) = List.nub ps'' :=>: ty'

      -- Compressing types in the generated map
      m' <- liftIO $ mapM (firstM compressQual) $ List.nub $ concat m2

      -- Operating black magic to get the final assumptions
      let names = getMapNames m'
      let as' = keepAssumpWithName (concat as) names
      (sub, as'') <- removeDuplicatesAssumps as'
      (remainingSub, _) <- removeDuplicatesAssumps as''
      as''' <- removeSuperclassAssumps as'' scs

      -- Getting the final expressions
      let finalExprs = map (second getAllElements) m'
      let finalExprs' = concatMap (\(_, e) -> map (\case
              Post.EVariable n t' -> (n, t')
              _ -> error "Not a variable"
            ) e) finalExprs

      -- Getting the duplicates assumptions in order to remove and resolve
      -- duplicatas.
      let finalExprs'' = map (\(MkIdentifier n _, Identity t') -> (n, t')) finalExprs'
      sub' <- unify finalExprs''
      let sub'' = Map.toList sub' <> sub <> remainingSub

      -- Running the expression reader because we're toplevel or there are 
      -- used-given generics.
      pos <- fetchPosition
      oldScs <- readIORef superclasses
      writeIORef superclasses scs
      h' <- liftIO $ runReaderT h $ getExpr pos m'
      writeIORef superclasses oldScs

      -- Substituting the duplicated assumptions in the expression
      let h'' = List.foldl substituteVar h' sub''
      
      -- Checking if there are some remaining assumptions in the scope.
      unlessM (allIsSuperclass as''' scs) $ throw (UnresolvedTypeVariable as''')
      
      -- Generating new types and expressions based on assumptions
      let args = map (\(n :>: t') -> fromText n :@: Identity t') as''
      let tys' = map (\(_ :>: t') -> t') as''
    
      cTy' <- liftIO $ compressPaths ty'

      let clos = if null args then h'' else Post.EClosure args (Identity cTy') h'' False
      let closTy = if null args then t else tys' :->: t

      let scheme' = Forall qvars ty''

      return (pure clos, closTy, [], scheme')

    exitLevel
  
    -- Creating a new scheme for the variable based on the substitution
    -- newScheme <- liftIO $ compressPaths t
    insertEnv @"typeEnv" name.identifier sch

    -- Removing the generic types from the environment
    mapM_ (deleteEnv @"genericsEnv" . getGenericName) generics

    -- Infer the body of the declaration
    b <- maybeM body (local id . infer)
    let retTy = maybe TUnit fst3 b
    let body' = mapM thd3 b
    let psb = maybe [] snd3 b

    let closTy' = Identity closTy

    -- print (name, isMut', isMut)

    pure (retTy, psb <> remainingPs, declFun (Annotation name closTy' isMut') <$> clos <*> body')
synthDecl _ _ _ = throw $ CompilerError "Only declarations are supported"

-- removeGeneralizedQuals :: [PlumeQualifier] -> [QuVar] -> IO [PlumeQualifier]
-- removeGeneralizedQuals [] _ = pure []
-- removeGeneralizedQuals qs [] = filterM removeQualWithQVar qs
-- removeGeneralizedQuals (IsIn (TypeQuantified n) _: qs) qvars 
--   | n `elem` qvars = removeGeneralizedQuals qs qvars
-- removeGeneralizedQuals (q : qs) qvars = (q:) <$> removeGeneralizedQuals qs qvars

-- removeQualWithQVar :: PlumeQualifier -> IO Bool
-- removeQualWithQVar (IsIn (TypeQuantified _) _) = pure False
-- removeQualWithQVar (IsIn (TypeVar l) n) = do
--   v <- readIORef l
--   case v of
--     Link t -> removeQualWithQVar (IsIn t n)
--     _ -> pure True
-- removeQualWithQVar _ = pure True

isTypeSubsetOf :: [PlumeType] -> [PlumeType] -> IO Bool
isTypeSubsetOf [] _ = pure True
isTypeSubsetOf (x : xs) ys = do
  res <- anyM (x `doesUnifyWith`) ys
  if res then isTypeSubsetOf xs ys else pure False

keepAssumpWithQual :: [Assumption PlumeType] -> [PlumeQualifier] -> IO [Assumption PlumeType]
keepAssumpWithQual as (q:qs) = do
  let t' = getDictTypeForPred q
  as1 <- filterM (\(_ :>: t) -> doesUnifyWith t' t) as
  as2 <- keepAssumpWithQual as qs
  pure . List.nub $ as1 <> as2
keepAssumpWithQual _ _ = pure []

removeSameQualifiers :: MonadChecker m => [QuVar] -> [PlumeQualifier] -> m [PlumeQualifier]
removeSameQualifiers _ [] = pure []
removeSameQualifiers qvs (q : qs) = do
  qs' <- removeSameQualifiers qvs qs
  b1 <- isElemOf q qs'
  b2 <- contains q qvs
  if b1 && b2 then pure qs' else pure (q : qs')

contains :: MonadChecker m => PlumeQualifier -> [QuVar] -> m Bool
contains (IsIn t _) qs = do
  t' <- liftIO $ mapM compressPaths t
  let qs' = map TypeQuantified qs
  anyM (\q -> and <$> zipWithM doesMatch t' (repeat q)) qs'
contains _ _ = pure False

isElemOf :: MonadChecker m => PlumeQualifier -> [PlumeQualifier] -> m Bool
isElemOf q (q':qs) = doesMatchQual q q' >>= \case
  True -> pure True
  False -> isElemOf q qs
isElemOf _ _ = pure False