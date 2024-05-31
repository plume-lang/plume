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
import Plume.Syntax.Concrete qualified as CST


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
  (Pre.ETypeExtension generics (Annotation tcName [tcTy] _) _ methods) = do
    -- Dealing with pre-types and building the qualified qualifiers
    -- for the typeclass instance (used to indicate the instance form and its
    -- superclasses)
    gens' :: [Post.PlumeQualifier] <- concatMapM convert generics

    let gens = removeQVars gens'
    let qvars = getQVars gens'

    ty <- convert tcTy
    let instH = IsIn ty tcName.identifier
    let pred' = gens :=>: instH
    
    -- Checking if the instance is already defined
    possibleInst <- isInstanceAlreadyDefined instH
    when (isJust possibleInst) $ case possibleInst of
      Just ty' -> throw $ AlreadyDefinedInstance tcName.identifier ty'
      _ -> throw $ CompilerError "Instance already defined"

    -- Pre-generating an instance dictionary only based on types
    cls@(MkClass _ _ meths) <- findClass tcName.identifier
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

    let schs = Map.map (\(Forall qs (quals :=>: ty')) -> Forall (qs <> qvars) ((quals <> gens) :=>: ty')) meths
    -- Typechecking the instance methods
    exprs <- zipWithM (extMemberToDeclaration infer) (Map.elems schs) methods

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
    let args' = map (\(n :>: t') -> Cmm.fromText n :@: Identity t') ass'
    let tys'' = map (\(_ :>: t') -> t') ass'

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

      -- Removing common superclasses between user-given extensions and found
      -- extensions in the inferred expression.
      (__ps, scs) <- case Map.lookup name schs of
        Just (Forall _ (ps2 :=>: _)) -> do
          p1 <- removeSuperclasses ps ps2
          p2 <- removeSuperclasses ps gens

          return (List.nub $ p1 <> p2, List.nub $ ps2 <> gens)
        Nothing -> pure ([], [])

      -- Getting all the needed qualifiers to qualify further the expression
      res' <- traverse (discharge cenv) ps
      let (_ps, m2, as, _) = unzip4 res'
      let ps' = concatMap removeQVars _ps

      ps'' <- removeDuplicatesQuals ps'
      let ty''@(_ :=>: t) = List.nub ps'' :=>: ty'

      -- Compressing types in the generated map
      m' <- liftIO $ mapM (firstM compressQual) $ List.nub $ concat m2
      let fstM1' = map fst m1'
      let m'' = filter ((`isQualNotDefined` fstM1') . fst) m'
      let finalM = m'' <> m1'

      -- Operating black magic to get the final assumptions
      let names = getMapNames m''
      let as' = keepAssumpWithName (concat as) names
      (sub, as'') <- removeDuplicatesAssumps as'
      (remainingSub, _) <- removeDuplicatesAssumps (as'' <> ass')
      as''' <- removeSuperclassAssumps as'' scs

      -- Getting the final expressions
      let finalExprs = map (second getAllElements) finalM
      let finalExprs' = concatMap (\(_, e) -> map (\case
              Post.EVariable n t' -> (n, t')
              _ -> error "Not a variable"
            ) e) finalExprs

      -- Getting the duplicates assumptions in order to remove and resolve
      -- duplicatas.
      let finalExprs'' = map (\(i, Identity t') -> (i.identifier, t')) finalExprs'
      sub' <- unify finalExprs''
      let sub'' = Map.toList sub' <> sub <> remainingSub

      -- Running the expression reader because we're toplevel or there are 
      -- used-given generics.
      pos <- fetchPosition
      oldScs <- readIORef superclasses
      writeIORef superclasses scs
      h' <- liftIO $ runReaderT h $ getExpr pos finalM
      writeIORef superclasses oldScs

      -- Substituting the duplicated assumptions in the expression
      let h'' = List.foldl substituteVar h' sub''

      -- Checking if there are some remaining assumptions in the scope.
      unlessM (allIsSuperclass as''' scs) $ throw (UnresolvedTypeVariable as''')

      -- Generating new types and expressions based on assumptions
      let args = map (\(n :>: t') -> Cmm.fromText n :@: Identity t') as'''
      let tys' = map (\(_ :>: t') -> t') as'''

      cTy' <- liftIO $ compressPaths ty'

      let clos = if null args then h'' else Post.EClosure args (Identity cTy') h''
      let closTy = if null args then t else tys' :->: t

      return ((Map.singleton name (Forall qvars ty''), closTy), Map.singleton name clos)

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

    -- Creating the new instance to insert it in the extend environment
    let inst = MkInstance qvars pred' dict dictFunTys
    let cls'' = (instH, void inst)
    addClassInstance cls''

    -- Generating the instance dictionary by sorting the methods by name
    ty' <- liftIO $ compressPaths ty
    let name = tcName.identifier <> "_" <> createInstName ty'
    let methods' = Map.toList dict
    let methods'' = sortBy (\(a, _) (b, _) -> compare a b) methods'

    -- Creating the instance expression and adding eventual super-interfaces
    -- constraints
    let tapp = TypeApp (TypeId tcName.identifier) [ty]
    let funTy = if null args' then tapp else tys'' :->: ty
    let instDict = Post.EInstanceDict tcName.identifier (Identity ty) (map snd methods'')

    let dictE = if null args' then instDict else Post.EClosure args' (Identity ty) instDict

    mapM_ (deleteEnv @"genericsEnv" . Cmm.getGenericName) generics

    pure (TUnit, [], pure (Post.EDeclaration [] (Annotation (Cmm.fromText name) (Identity funTy) False) dictE Nothing))
synthExt _ _ = throw $ CompilerError "Only type extensions are supported"

getExpr :: CST.Position -> [(PlumeQualifier, Post.Expression)] -> PlumeQualifier -> Post.Expression
getExpr pos xs p = do
  let p' = unsafePerformIO $ compressQual p
  case List.lookup p' xs of
    Just x -> x
    Nothing -> unsafePerformIO $ do
      interpretError (pos, CompilerError $ "getExpr: " <> show p' <> " not found in " <> show xs)
      exitFailure

removeSuperclassAssumps :: MonadIO m => [Assumption PlumeType] -> [PlumeQualifier] -> m [Assumption PlumeType]
removeSuperclassAssumps [] _ = pure []
removeSuperclassAssumps (x@(_ :>: TypeApp (TypeId tcName) [ty]) : xs) scs = do
  ty' <- liftIO $ compressPaths ty
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

  (bTy, ps', b) <- local id $ infer c
  ty' `unifiesWith` bTy

  mapM_ (deleteEnv @"genericsEnv" . Cmm.getGenericName) gens
  pure (bTy, ps', b, name.identifier)

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
allIsSuperclass (_ :>: TypeApp (TypeId tc) [ty] : xs) ps = do
  let name = toString tc
  case name of
    '@':tcName -> do
      let inst = IsIn ty (fromString tcName)
      b <- isSuperClass inst ps
      if b then allIsSuperclass xs ps else pure False
    _ -> allIsSuperclass xs ps
allIsSuperclass (_ : xs) ps = allIsSuperclass xs ps