{-# LANGUAGE LambdaCase #-}

module Plume.TypeChecker.Checker.Extension where

import Data.List (unzip4)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Plume.Syntax.Abstract qualified as Pre
import GHC.IO hiding (liftIO)
import Plume.Syntax.Common qualified as Cmm
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Translation.Generics (concatMapM)
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Solver
import Plume.TypeChecker.Constraints.Typeclass
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.Monad.Free (substituteVar)
import Plume.TypeChecker.Monad.Type qualified as Post
import Plume.TypeChecker.TLIR qualified as Post
import Prelude hiding (gets, local)


firstM :: (Monad m) => (a -> m b) -> (a, c) -> m (b, c)
firstM f (a, b) = do
  a' <- f a
  pure (a', b)

type Converted = Either PlumeType PlumeType

type VarSubst = Map Text Text

unifyQual :: MonadChecker m => (Text, PlumeType) -> (Text, PlumeType) -> m VarSubst
unifyQual (n1, t1) (n2, t2) = liftIO $ do
  t1' <- compressPaths t1
  t2' <- compressPaths t2
  if t1' == t2'
    then pure $ Map.singleton n1 n2
    else pure mempty

unify :: MonadChecker m => [(Text, PlumeType)] -> m VarSubst
unify [] = pure mempty
unify (x : xs) = do
  s <- mapM (unifyQual x) xs
  let s' = mconcat s
  s'' <- unify xs
  pure $ s' <> s''

getVarName :: Post.Expression -> Text
getVarName (Post.EVariable n _) = n
getVarName _ = ""

getVarType :: Post.Expression -> PlumeType
getVarType (Post.EVariable _ t) = t
getVarType _ = error "getVarType: Not a variable"

getAllElements :: Post.Expression -> [Post.Expression]
getAllElements (Post.EApplication f xs) = f : concatMap getAllElements xs
getAllElements x = [x]

isQualNotDefined :: PlumeQualifier -> [PlumeQualifier] -> Bool
isQualNotDefined _ [] = True
isQualNotDefined (IsIn t1 n1) ((IsIn t2 n2) : xs) 
  | n1 == n2 && t1 == t2 = False
  | otherwise = isQualNotDefined (IsIn t1 n1) xs
isQualNotDefined t (_ : xs) = isQualNotDefined t xs

keepAssumpWithName :: [Assumption PlumeType] -> [Text] -> [Assumption PlumeType]
keepAssumpWithName [] _ = []
keepAssumpWithName (x@(n :>: _) : xs) names
  | n `elem` names = x : keepAssumpWithName xs names
  | otherwise = keepAssumpWithName xs names

getMapNames :: [(PlumeQualifier, Post.Expression)] -> [Text]
getMapNames ((IsIn _ _, Post.EVariable n _) : xs) = n : getMapNames xs
getMapNames (_ : xs) = getMapNames xs
getMapNames [] = []

isInstanceAlreadyDefined :: MonadChecker m => PlumeQualifier -> m (Maybe PlumeType)
isInstanceAlreadyDefined p = do
  p' <- liftIO $ compressQual p
  MkExtendEnv instances <- gets (extendEnv . environment)

  found <- findM instances $ \case
    (p'', _) -> do
      p''' <- liftIO $ compressQual p''
      doesMatchQual p''' p'

  case found of
    Just (IsIn ty _, _) -> return (Just ty)
    _ -> return Nothing

synthExt :: Infer -> Infer
synthExt
  infer
  (Pre.ETypeExtension generics (Annotation tcName [tcTy]) _ methods) = do
    -- Dealing with pre-types and building the qualified qualifiers
    -- for the typeclass instance (used to indicate the instance form and its
    -- superclasses)
    gens' :: [Post.PlumeQualifier] <- concatMapM convert generics

    let gens = removeQVars gens'
    let qvars = getQVars gens'

    ty <- convert tcTy
    let instH = IsIn ty tcName
    let pred' = gens :=>: instH
    
    possibleInst <- isInstanceAlreadyDefined instH
    when (isJust possibleInst) $ case possibleInst of
      Just ty' -> throw $ AlreadyDefinedInstance tcName ty'
      _ -> throw $ CompilerError "Instance already defined"

    cls@(MkClass _ _ meths) <- findClass tcName
    case cls of
      MkClass qs' quals methods' -> do
        case quals of 
          _  :=>: t -> do
            b <- liftIO $ doesQualUnifiesWith t instH 
            unless b $ throw $ CompilerError "Instance does not match the class"

        let finalMethods = fmap (\(Forall qs (quals' :=>: ty')) -> Forall (qs <> qs') $ (gens <> quals') :=>: ty') methods'

        let quals' = case quals of xs :=>: t -> (xs <> gens) :=>: t

        let preInst = MkInstance qvars quals' mempty finalMethods

        addClassInstance (instH, void preInst)

    -- Typechecking the instance methods
    exprs <- mapM (extMemberToDeclaration infer) methods

    cenv <- gets (extendEnv . environment)

    -- Generating the required instance constraints arguments, for instance
    -- show<[a]> requires show<a> to be in scope, resulting in adding show<a>
    -- dictionary as a lambda argument to show<[a]>
    --
    -- Would compile to: show<[a]> = \show<a> -> methods..
    res'' <- traverse (discharge cenv) gens
    let (_, m1, ass, _) = unzip4 res''
    m1' <- liftIO . mapM (firstM compressQual) . List.nub $ concat m1
    let ass' = concat ass
    let args' = map (\(n :>: t') -> n :@: t') ass'
    let tys'' = map getAssumpVal ass'

    res' <- forM exprs $ \(ty', ps, h, name) -> do
      -- Generating the right local method constraints instances, for instance
      -- if `test` method in `test<a>` class is of the form:
      --   test :: show<a> => a -> int
      -- then the show<a> constraint should be transformed into a dictionary
      -- lambda argument `show<a>`, giving a method:
      --   test :: show<a> -> a -> int
      --
      -- Instance superclases should not interfere with method-local instances
      -- to avoid duplicate instances.
      res' <- traverse (discharge cenv) ps
      let (_ps, m2, as, _) = unzip4 res'
      let ps' = concatMap removeQVars _ps

      ps'' <- removeDuplicatesQuals ps'

      let ty''@(_ :=>: t) = List.nub ps'' :=>: ty'

      m' <- liftIO $ mapM (firstM compressQual) $ List.nub $ concat m2
      let fstM1' = map fst m1'
      let m'' = filter ((`isQualNotDefined` fstM1') . fst) m'

      let names = getMapNames m''
      let as' = keepAssumpWithName (concat as) names

      (sub, as'') <- removeDuplicatesAssumps as'

      let finalM = m'' <> m1'

      let finalExprs = map (second getAllElements) finalM
      let finalExprs' = concatMap (\(_, e) -> map (\case
              Post.EVariable n t' -> (n, t')
              _ -> error "Not a variable"
            ) e) finalExprs

      sub' <- unify finalExprs'

      let sub'' = Map.toList sub' <> sub

      h' <- liftIO $ runReaderT h $ getExpr finalM
      let h'' = List.foldl substituteVar h' sub''

      unless (null as'') $ throw (UnresolvedTypeVariable as'')

      let args = map (\(n :>: t') -> n :@: t') as''
      let tys' = map getAssumpVal as''

      cTy' <- liftIO $ compressPaths ty'

      pos <- fetchPosition

      let clos = if null args then h'' else Post.EClosure args cTy' h'' pos
      let closTy = if null args then t else tys' :->: t

      return ((Map.singleton name (Forall qvars ty''), closTy), Map.singleton name clos)

    let (quals, exprs') = unzip res'
    let (funTys, _) = unzip quals

    let dict = Map.unions exprs'
    let dictFunTys = Map.unions funTys

    let meths' = Map.keys dict
    let missingMeths = Map.keysSet meths `Set.difference` Set.fromList meths'

    unless (Set.null missingMeths) $ 
      throw $ MissingExtensionMethods tcName (Set.toList missingMeths)

    let inst = MkInstance qvars pred' dict dictFunTys
    let cls'' = (instH, void inst)
    addClassInstance cls''

    ty' <- liftIO $ compressPaths ty
    let name = tcName <> "_" <> createInstName ty'
    let methods' = Map.toList dict
    let methods'' = sortBy (\(a, _) (b, _) -> compare a b) methods'

    let tapp = TypeApp (TypeId tcName) [ty]
    let funTy = if null args' then tapp else tys'' :->: ty
    let instDict = Post.EInstanceDict tcName ty (map snd methods'')

    pos <- fetchPosition
    let dictE = if null args' then instDict else Post.EClosure args' ty instDict pos

    mapM_ (deleteEnv @"genericsEnv" . Cmm.getGenericName) generics

    pure (TUnit, [], pure (Post.EDeclaration (Annotation name funTy) dictE Nothing))
    where
      getAssumpVal :: Assumption a -> a
      getAssumpVal (_ :>: val) = val
synthExt _ _ = throw $ CompilerError "Only type extensions are supported"

getExpr :: [(PlumeQualifier, Post.Expression)] -> PlumeQualifier -> Post.Expression
getExpr xs p = do
  let p' = unsafePerformIO $ compressQual p
  case List.lookup p' xs of
    Just x -> x
    Nothing -> error $ "getExpr: " <> show p' <> " not found in " <> show xs

extMemberToDeclaration :: (MonadChecker m) => Infer -> Pre.ExtensionMem -> m (PlumeType, [PlumeQualifier], Placeholder Post.Expression, Text)
extMemberToDeclaration infer (Pre.ExtDeclaration gens (Annotation name ty) c) = do
  _ :: [PlumeQualifier] <- concatMapM convert gens
  ty' <- convert ty

  searchEnv @"typeEnv" name >>= \case
    Just sch -> do
      instantiate sch >>= \case
        (ty'', _) -> ty' `unifiesWith` ty''
    Nothing -> throw $ UnboundVariable name
  -- addClassInstance

  (bTy, ps', b) <- local id $ infer c
  ty' `unifiesWith` bTy

  mapM_ (deleteEnv @"genericsEnv" . Cmm.getGenericName) gens
  pure (bTy, ps', b, name)

getExtMemberNames :: Pre.ExtensionMem -> Text
getExtMemberNames (Pre.ExtDeclaration _ (Annotation name _) _) = name

getConverted :: Converted -> PlumeType
getConverted = \case
  Left ty -> ty
  Right ty -> ty

convertMaybe :: (a `ConvertsTo` b) => Maybe a -> Checker (Either PlumeType b)
convertMaybe = \case
  Just x -> Right <$> convert x
  Nothing -> Left <$> fresh
