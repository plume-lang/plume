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
import Plume.TypeChecker.Monad.Type qualified as Post
import Plume.TypeChecker.TLIR qualified as Post
import Prelude hiding (gets, local)
import Plume.Syntax.Concrete qualified as CST
import qualified Plume.TypeChecker.Monad as M

assocSchemes :: MonadChecker m => [(Text, PlumeScheme)] -> [Pre.ExtensionMember] -> m [(PlumeScheme, Pre.ExtensionMember)]
assocSchemes _ [] = pure []
assocSchemes env (e@(Pre.ExtDeclaration _ (Annotation name _ _) _) : xs) = do
  let name' = name.identifier
  case List.lookup name' env of
    Just sch -> ((sch, e) :) <$> assocSchemes env xs
    Nothing -> throw $ CompilerError "Scheme not found"


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

combineAssumps :: [Assumption PlumeType] -> [Assumption PlumeType] -> [Assumption PlumeType]
combineAssumps [] xs = xs
combineAssumps xs [] = xs
combineAssumps (x@(_ :>: t) : xs) ys = do
  let found = findMatching t ys

  case found of
    Nothing -> x : combineAssumps xs ys
    Just x' -> x : combineAssumps xs (filter (/= x') ys)

  where
    findMatching :: PlumeType -> [Assumption PlumeType] -> Maybe (Assumption PlumeType)
    findMatching _ [] = Nothing
    findMatching t1 (x'@(_ :>: t2) : xs')
      | t1 == t2 = Just x'
      | otherwise = findMatching t xs'

getVarName :: Post.Expression -> Text
getVarName (Post.EVariable n _) = n.identifier
getVarName _ = ""

getVarType :: Post.Expression -> PlumeType
getVarType (Post.EVariable _ (Identity t)) = t
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
getMapNames ((IsIn _ _, Post.EVariable n _) : xs) = n.identifier : getMapNames xs
getMapNames (_ : xs) = getMapNames xs
getMapNames [] = []

isInstanceAlreadyDefined :: MonadChecker m => PlumeQualifier -> m (Maybe [PlumeType])
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

applyQualifier :: MonadChecker m => Substitution -> PlumeQualifier -> m PlumeQualifier
applyQualifier subst (IsIn tys name) = do
  tys' <- mapM (applySubst subst) tys
  pure $ IsIn tys' name
applyQualifier _ (IsQVar qv) = pure $ IsQVar qv

applyQualified :: MonadChecker m => Substitution -> Qualified PlumeQualifier -> m (Qualified PlumeQualifier)
applyQualified subst (ps :=>: t) = do
  ps' <- mapM (applyQualifier subst) ps
  t' <- applyQualifier subst t
  pure $ ps' :=>: t'

applyScheme :: MonadChecker m => Substitution -> PlumeScheme -> m PlumeScheme
applyScheme subst (Forall qv (ps :=>: t)) = do
  ps' <- mapM (applyQualifier subst) ps
  t' <- applySubst subst t

  let substQVS = Map.keys subst
  let qv' = Set.difference qv (Set.fromList substQVS)

  pure $ Forall qv' (ps' :=>: t')

instantiateClass :: MonadChecker m => Class -> m Class
instantiateClass (MkClass qvs pred' members _) = do
  qvs' <- mapM (const fresh) qvs
  let subst = Map.fromList $ zip qvs qvs'

  pred'' <- applyQualified subst pred'
  members' <- mapM (applyScheme subst) members

  pure $ MkClass [] pred'' members' Nothing

findInstance :: MonadChecker m => PlumeQualifier -> m (Maybe (Instance Post.Expression ()))
findInstance p = do
  p' <- liftIO $ compressQual p
  MkExtendEnv instances <- gets (extendEnv . environment)

  found <- findM instances $ \case
    (p'', _) -> do
      p''' <- liftIO $ compressQual p''
      doesMatchQual p''' p'

  case found of
    Just (_, inst) -> return (Just inst)
    _ -> return Nothing


removeDuplicatesPSs :: MonadChecker m => [PlumeQualifier] -> m [PlumeQualifier]
removeDuplicatesPSs [] = pure []
removeDuplicatesPSs (p@(IsIn _ _) : xs) = do
  findInstance p >>= \case
    Just (MkInstance _ (preds :=>: _) _ _) -> do
      mapM_ (liftIO . compressQual) preds
      let preds' = Set.fromList (filter (`notElem` preds) xs)

      (p:) <$> removeDuplicatesPSs (Set.toList preds')
    _ -> (p:) <$> removeDuplicatesPSs xs
removeDuplicatesPSs (x : xs) = (x:) <$> removeDuplicatesPSs xs

synthExt :: Infer -> Infer
synthExt
  infer
  (Pre.ETypeExtension generics (Annotation tcName tcTy _) _ methods) = do
    -- Dealing with pre-types and building the qualified qualifiers
    -- for the typeclass instance (used to indicate the instance form and its
    -- superclasses)
    gens' :: [Post.PlumeQualifier] <- concatMapM convert generics

    let gens = removeQVars gens'
    let qvars = getQVars gens'

    ty <- mapM convert tcTy
    let instH = IsIn ty tcName.identifier
    let pred' = gens :=>: instH

    -- Checking if the instance is already defined
    possibleInst <- isInstanceAlreadyDefined instH
    when (isJust possibleInst) $ case possibleInst of
      Just ty' -> throw $ AlreadyDefinedInstance tcName.identifier ty'
      _ -> throw $ CompilerError "Instance already defined"

    tys' <- mapM convert tcTy

    let inst = IsIn tys' tcName.identifier
    
    -- Pre-generating an instance dictionary only based on types
    cls@(MkClass _ _ meths ded) <- findClass tcName.identifier >>= instantiateClass

    case cls of
      MkClass _ (_ :=>: inst') _ _ -> unifiyTyQualWith inst inst'

    fdSub <- case cls of
      MkClass qs' quals methods' _ -> do
        case quals of
          _  :=>: t -> do
            b <- liftIO $ doesQualUnifiesWith t instH
            unless b $ throw $ CompilerError "Instance does not match the class"

        let finalMethods = fmap (\(Forall qs (quals' :=>: ty')) -> Forall (qs <> Set.fromList qs') $ (gens <> quals') :=>: ty') methods'

        let quals' = case quals of xs :=>: t -> (xs <> gens) :=>: t
        
        let preInst :: Instance Post.Expression PlumeQualifier = MkInstance qvars quals' mempty finalMethods

        let qs'' = TypeQuantified <$> qs'
        sub <- liftIO . composeSubs =<< zipWithM unifyAndGetSub qs'' ty

        addClassInstance (inst, void preInst)

        pure sub

    -- Getting deduction type correspondance
    let (ded', dedTy') = case ded of
          Just (TypeQuantified ded'', TypeQuantified dedTy'') -> 
            (Map.lookup ded'' fdSub, Map.lookup dedTy'' fdSub)
          _ -> 
            (Nothing, Nothing)

    -- Adding functional dependencies to the state
    case (ded', dedTy') of
      (Just ded'', Just dedTy'') -> do

        M.modify $ \s -> s {
          environment = s.environment {
            funDeps = Map.insert ded'' (tcName.identifier, dedTy'') s.environment.funDeps
          }
        }
      _ -> pure ()

    -- let meths' = Map.map (\(Forall qs (quals :=>: ty')) -> Forall qs ((inst:quals) :=>: ty')) meths
    assocs <- assocSchemes (Map.toList meths) methods

    -- Typechecking the instance methods
    exprs <- traverse (uncurry $ extMemberToDeclaration infer) assocs
    
    cenv <- gets (extendEnv . environment)

    -- Generating the required instance constraints arguments, for instance
    -- show<[a]> requires show<a> to be in scope, resulting in adding show<a>
    -- dictionary as a lambda argument to show<[a]>
    --
    -- Would compile to: show<[a]> = \show<a> -> methods..
    res'' <- traverse (discharge cenv) gens
    let (_, m1, ass, _) = unzip4 res''
    let m1' = Map.unions m1
    let ass' = concat ass
    ass'' <- removeTypeVars ass'
    
    let args' = map (\(n :>: t') -> Cmm.fromText n :@: Identity t') ass''
    let tys'' = map (\(_ :>: t') -> t') ass''

    res' <- forM exprs $ \(ty', ps, h, name) -> do
      ps' <- liftIO $ removeTypeVarPS ps >>= removeDuplicatesPS
      _ps' <- List.nub . (List.\\ gens) <$> removeDuplicatesPSs ps'
      zs <- traverse (discharge cenv) _ps'
      
      let (ps'', m, as, _) = mconcat zs

      ps''' <- liftIO $ removeTypeVarPS ps'' >>= removeDuplicatesPS

      -- ps'' <- removeDuplicatesQuals ps'
      let t''@(_ :=>: t) = List.nub ps''' :=>: ty'

      pos <- fetchPosition
      checkSub <- gets substitution

      h' <- liftIO $ runReaderT h $ getExpr pos checkSub (m1' <> m)
    
      let removeDuplicate :: [PlumeType] -> [Assumption PlumeType] -> [Assumption PlumeType]
          removeDuplicate xs (a@(_ :>: t'):xs') 
            | t' `elem` xs = removeDuplicate xs xs'
            | otherwise = a : removeDuplicate xs xs'
          removeDuplicate _ [] = []

      let tmpAssumps = map getDictTypeForPred gens
          newAssumps = removeDuplicate tmpAssumps as

      as'' <- removeTypeVars newAssumps

      unless (null as'') $ do
        throw (UnresolvedTypeVariable as'')

      remainingAssumps <- removeTypeVars as

      remainingAssumps' <- liftIO $ removeDuplicatesAssumps' remainingAssumps

      -- Generating new types and expressions based on assumptions
      let args = map (\(n :>: t') -> Cmm.fromText n :@: Identity t') remainingAssumps'
      let tys''' = map (\(_ :>: t') -> t') remainingAssumps'

      cTy' <- liftIO $ compressPaths ty'

      let clos = if null args then h' else Post.EClosure args (Identity cTy') h' False
      let closTy = if null args then t else tys''' :->: t

      return ((Map.singleton name (Forall (Set.fromList qvars) t''), closTy), Map.singleton name clos)

    let (quals, exprs') = unzip res'
    let (funTys, _) = unzip quals

    -- Creating instance dictionary
    let dict = Map.unions exprs'
    let dictFunTys = Map.unions funTys

    -- Checking for missing methods in the extension (resp. instance)
    let meths' = Map.keys dict
    let areThereMissingMethods = Set.fromList meths' `Set.isSubsetOf` Map.keysSet meths
    let missingMeths =
          if areThereMissingMethods
            then Set.fromList meths' Set.\\ Map.keysSet meths
            else Set.empty

    unless (Set.null missingMeths) $
      throw $ MissingExtensionMethods tcName.identifier (Set.toList missingMeths)

    let unknown = filter (`Map.notMember` meths) meths'

    unless (null unknown) $
      throw $ UnknownExtensionMethods tcName.identifier unknown

    -- Creating the new instance to insert it in the extend environment
    let inst' = MkInstance qvars pred' dict dictFunTys
    let cls'' = (instH, void inst')
    addClassInstance cls''

    -- Generating the instance dictionary by sorting the methods by name
    ty' <- liftIO $ mapM compressPaths ty
    let name = tcName.identifier <> "_" <> createInstNames ty'
    let methods' = Map.toList dict
    let methods'' = sortBy (\(a, _) (b, _) -> compare a b) methods'

    -- Creating the instance expression and adding eventual super-interfaces
    -- constraints
    let tapp = TypeApp (TypeId tcName.identifier) ty
    let funTy = if null args' then tapp else tys'' :->: tapp
    let instDict = Post.EInstanceDict tcName.identifier (Identity tapp) (map snd methods'')

    let dictE = if null args' then instDict else Post.EClosure args' (Identity tapp) instDict False

    mapM_ (deleteEnv @"genericsEnv" . Cmm.getGenericName) generics

    pure (TUnit, [], pure (Post.EDeclaration [] (Annotation (Cmm.fromText name) (Identity funTy) False) dictE Nothing), False)
synthExt _ _ = throw $ CompilerError "Only type extensions are supported"

getExpr :: CST.Position -> M.Substitution -> Map PlumeQualifier Post.Expression -> PlumeQualifier -> Post.Expression
getExpr pos sub xs p = do
  let p' = unsafePerformIO (applyQual sub =<< compressQual p)
  case Map.lookup p' xs of
    Just x -> x
    Nothing -> unsafePerformIO $ do
      interpretError (pos, CompilerError $ "getExpr: " <> show p' <> " not found in " <> show xs)
      exitFailure

removeSuperclassAssumps :: MonadIO m => [Assumption PlumeType] -> [PlumeQualifier] -> m [Assumption PlumeType]
removeSuperclassAssumps [] _ = pure []
removeSuperclassAssumps (x@(_ :>: TypeApp (TypeId tcName) ty) : xs) scs = do
  ty' <- liftIO $ mapM compressPaths ty
  let tcName' = toString tcName
  case tcName' of
    '@':tcName'' -> do
      let tcName''' = fromString tcName''
      found <- filterM (liftIO . doesQualUnifiesWith (IsIn ty' tcName''')) scs
      case found of
        [] -> (x:) <$> removeSuperclassAssumps xs scs
        _ -> removeSuperclassAssumps xs scs
    _ -> removeSuperclassAssumps xs scs
removeSuperclassAssumps (x : xs) scs = (x:) <$> removeSuperclassAssumps xs scs

-- | Inference for extension members. Extension members are just regular
-- | expressions or functions, except that they are part of a type extension,
-- | so they might have super-constraints implied by the upper extension.
extMemberToDeclaration :: (MonadChecker m) => Infer -> PlumeScheme -> Pre.ExtensionMember -> m (PlumeType, [PlumeQualifier], Placeholder Post.Expression, Text)
extMemberToDeclaration infer sch (Pre.ExtDeclaration gens (Annotation name ty _) c) = do
  _ :: [PlumeQualifier] <- concatMapM convert gens
  ty' <- convert ty

  instantiate sch >>= \case
    (ty'', _) -> ty' `unifiesWith` ty''

  (bTy, ps', b, _) <- local id $ infer c
  ty' `unifiesWith` bTy

  mapM_ (deleteEnv @"genericsEnv" . Cmm.getGenericName) gens

  ps'' <- removeTypeVarsQ ps'

  pure (bTy, ps'', b, name.identifier)

-- | Checking if an interface is a included in a list of interfaces
isSuperClass :: MonadIO m => PlumeQualifier -> [PlumeQualifier] -> m Bool
isSuperClass p ps = do
  p' <- liftIO $ compressQual p
  ps' <- liftIO $ mapM compressQual ps
  found <- findMatchingClass p' ps'

  case found of
    [] -> pure False
    _ -> pure True

allIsSuperclass :: MonadIO m => [Assumption PlumeType] -> [PlumeQualifier] -> m Bool
allIsSuperclass [] _ = pure True
allIsSuperclass (_ :>: TypeApp (TypeId tc) ty : xs) ps = do
  let name = toString tc
  case name of
    '@':tcName -> do
      let inst = IsIn ty (fromString tcName)
      b <- isSuperClass inst ps
      if b then allIsSuperclass xs ps else pure False
    _ -> allIsSuperclass xs ps
allIsSuperclass (_ : xs) ps = allIsSuperclass xs ps

removeTypeVars :: MonadIO m => [Assumption PlumeType] -> m [Assumption PlumeType]
removeTypeVars [] = pure []
removeTypeVars ((x' :>: TypeApp _ [TypeVar tv]) : xs) = do
  v <- liftIO $ readIORef tv
  case v of
    Link t -> removeTypeVars ((x' :>: t) : xs)
    _ -> removeTypeVars xs
removeTypeVars (x : xs) = (x:) <$> removeTypeVars xs

removeTypeVarsQ :: MonadIO m => [PlumeQualifier] -> m [PlumeQualifier]
removeTypeVarsQ [] = pure []
removeTypeVarsQ (IsIn ts n : xs) = do
  b <- anyM containTV ts

  if b then removeTypeVarsQ xs else (IsIn ts n :) <$> removeTypeVarsQ xs
  where
    containTV :: MonadIO m => PlumeType -> m Bool
    containTV (TypeVar tv) = do
      v <- liftIO $ readIORef tv
      case v of
        Link t -> containTV t
        _ -> pure True
    containTV (TypeApp t ts') = do
      b <- containTV t
      if b then anyM containTV ts' else pure False
    containTV _ = pure False
removeTypeVarsQ (x : xs) = (x:) <$> removeTypeVarsQ xs