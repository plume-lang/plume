{-# LANGUAGE LambdaCase #-}
module Plume.TypeChecker.Constraints.Typeclass where

import Data.List qualified as List
import Data.Map qualified as Map
import Data.Foldable qualified as Fold
import Plume.Compiler.Desugaring.Monad (freshName)
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Solver (unifiesWith)
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.TLIR qualified as Post
import Plume.Syntax.Common.Annotation qualified as Cmm
import Prelude hiding (gets)

-- | Discharging operation is a step that decompose a qualified type into smaller
-- | extensions. This also generates the new dictionaries if no extensions is
-- | is found for a given type. And this also generates the expressions that will
-- | be used to create instance calls.
discharge ::
  (MonadChecker m) =>
  ExtendEnv ->
  PlumeQualifier ->
  m
    ( [PlumeQualifier],
      [(PlumeQualifier, Post.Expression)],
      [Assumption PlumeType],
      [Post.Expression]
    )
discharge cenv p = do
  -- Checking if some extension exists for the given qualifier and getting the
  -- first to match.
  p' <- liftIO $ compressQual p
  x <- forM (getQuals cenv) $ \(qvs, sch) -> do
    sub <- Map.fromList <$> mapM (\c -> (c,) <$> fresh) qvs
    (a :=>: b, _) <- instantiateQual sub sch
    b' <- liftIO $ compressQual b
    First <$> (fmap (a,b,) <$> matchMut' b' p') `tryOr` pure Nothing

  case getFirst $ mconcat x of
    Just (_ps, b, _) -> do
      -- Removing already deduced superclasses, e.g. boolean_algebra A is a
      -- superclass of equality A, so if both are presents, we can remove 
      -- boolean_algebra qualifier.
      let ps = List.nub $ removeQVars _ps
      _ps' <- removeSuperclassesQuals ps

      -- Recursively discharging environment in order to get smaller pieces of
      -- qualifiers
      (ps', mp, as, ds) <-
        fmap mconcat
          . mapM (discharge cenv)
          $ _ps'

      -- Getting dictionary stuffs for generating calls, map, assumptions..
      let ty = getDictTypeForPred p'
      t <- liftIO $ compressPaths ty

      let d = Post.EVariable (Cmm.fromText (getDict b)) (Identity t)
          e = if null ds then d else Post.EApplication d ds
      pure (ps', mp <> [(p', e)], as, pure e)
    Nothing -> do
      -- If no exact extension is found, backing off and creating a new
      -- dictionary for the extension. 
      param <- freshName
      let paramTy = getDictTypeForPred p'
      pure
        ( pure p,
          List.singleton (p', Post.EVariable (Cmm.fromText param) (Identity paramTy)),
          pure $ param :>: paramTy,
          pure $ Post.EVariable (Cmm.fromText param) (Identity paramTy)
        )

-- SOME TEXT QUALIFIER RELATED FUNCTIONS
getDictName :: Text -> PlumeType
getDictName n = TypeId $ getDictName2 n

getDictName2 :: Text -> Text
getDictName2 n = "@" <> n

getDictTypeForPred :: PlumeQualifier -> PlumeType
getDictTypeForPred (IsIn c t) = TypeApp (getDictName t) [c]
getDictTypeForPred (IsQVar t) = TypeQuantified t

getDict :: PlumeQualifier -> Text
getDict (IsIn c t) = t <> "_" <> createInstName c
getDict (IsQVar t) = t

getQuals :: ExtendEnv -> [([QuVar], Qualified PlumeQualifier)]
getQuals (MkExtendEnv env) = map (\(a, MkInstance qs quals _ _) -> (qs, a <$ quals)) env

unqualType :: Qualified PlumeType -> PlumeType
unqualType (_ :=>: zs) = zs

unqualScheme :: PlumeScheme -> PlumeType
unqualScheme (Forall _ t) = unqualType t

normalizeType2 :: PlumeType -> PlumeType
normalizeType2 = unqualScheme . normalize . (Forall [] . ([] :=>:))

normalize :: PlumeScheme -> PlumeScheme
normalize (Forall qs t) = Forall qs $ normqual t
  where
    normqual (xs :=>: zs) =
      fmap (\case
        (IsIn c t') -> IsIn (normtype c) t'
        _ -> error "Impossible") xs :=>: normtype zs

    normtype :: PlumeType -> PlumeType
    normtype (TypeId a) = TypeId a
    normtype (TypeApp a b) = TypeApp (normtype a) (map normtype b)
    normtype (TypeVar c) = TypeVar c
    normtype (TypeQuantified a) = TypeQuantified a

-- SOME UNIFYING FUNCTIONS FOR DISCHARGING
doesMatch :: (MonadChecker m) => PlumeType -> PlumeType -> m Bool
doesMatch (TypeApp x xs) (TypeApp y ys) = do
  b <- doesMatch x y
  if b
    then and <$> zipWithM doesMatch xs ys
    else pure False
doesMatch (TypeVar u) t = do
  v <- readIORef u
  case v of
    Link t' -> doesMatch t' t
    Unbound _ _ -> do
      writeIORef u (Link t)
      pure True
doesMatch (TypeQuantified _) _ = pure True
doesMatch (TypeId n) (TypeId n') = pure $ n == n'
doesMatch _ _ = pure False

doesMatchQual :: (MonadChecker m) => PlumeQualifier -> PlumeQualifier -> m Bool
doesMatchQual (IsIn a b) (IsIn a' b') = do
  a1 <- liftIO $ compressPaths a
  a2 <- liftIO $ compressPaths a'
  bl <- doesMatch a1 a2
  pure $ bl && b == b'
doesMatchQual _ _ = pure False

matchMut :: (MonadChecker m) => PlumeType -> PlumeType -> m ()
matchMut (TypeApp x xs) (TypeApp y ys) = do
  matchMut x y
  mconcat <$> zipWithM matchMut xs ys
matchMut (TypeVar u) t = do
  v <- readIORef u
  case v of
    Link t' -> matchMut t' t
    Unbound _ _ -> writeIORef u (Link t)
matchMut (TypeQuantified _) _ = pure ()
matchMut (TypeId n) (TypeId n') | n == n' = pure ()
matchMut t1 t2 = throw $ CompilerError $ "Type mismatch between " <> show t1 <> " and " <> show t2

matchMut' :: (MonadChecker m) => PlumeQualifier -> PlumeQualifier -> m (Maybe ())
matchMut' (IsIn a1 b) (IsIn a2 b') | b == b' = do
  a1' <- liftIO $ compressPaths a1
  a2' <- liftIO $ compressPaths a2
  Just <$> matchMut a1' a2'
matchMut' _ _ = pure Nothing

-- SOME QUALIFIER FUNCTIONS
removeDuplicatesQuals :: (MonadChecker m) => [PlumeQualifier] -> m [PlumeQualifier]
removeDuplicatesQuals [] = pure []
removeDuplicatesQuals (x@(IsIn (TypeQuantified _) tc) : xs) = do
  xs' <- removeQualsWithTVar tc xs
  pure (x : xs')
removeDuplicatesQuals (x : xs) = do
  xs' <- removeDuplicatesQuals xs
  b <- elemQual x xs'
  if b
    then pure xs'
    else pure (x : xs')

removeQualsWithTVar :: (MonadChecker m) => Text -> [PlumeQualifier] -> m [PlumeQualifier]
removeQualsWithTVar _ [] = pure []
removeQualsWithTVar n (x@(IsIn t@(TypeVar _) n') : xs) | n == n' = do
  t' <- liftIO $ compressPaths t
  case t' of
    TypeVar _ -> removeQualsWithTVar n xs
    _ -> pure (x : xs)
removeQualsWithTVar n (x : xs) = (x :) <$> removeQualsWithTVar n xs

elemQual :: MonadChecker m => PlumeQualifier -> [PlumeQualifier] -> m Bool
elemQual (IsIn a1 b) =
  anyM
    ( \case
      (IsIn a2 b') -> do
        a1' <- liftIO $ compressPaths a1
        a2' <- liftIO $ compressPaths a2
        bl <- case a1' of
          TypeQuantified _ -> pure False
          TypeVar _ -> case a2' of
            TypeQuantified _ -> pure True
            TypeVar _ -> pure True
            _ -> liftIO $ doesUnifyWith a1' a2'
          _ -> liftIO $ doesUnifyWith a1' a2'
        
        pure $ b == b' && bl
      _ -> pure False
    )
elemQual _ = pure . const False

removeDuplicatesAssumps :: (MonadChecker m) => [Assumption PlumeType] -> m ([(Text, Text)], [Assumption PlumeType])
removeDuplicatesAssumps [] = pure ([], [])
removeDuplicatesAssumps (x@(name :>: _) : xs) = do
  (repls, xs') <- removeDuplicatesAssumps xs
  b <- liftIO $ elemAs x xs'
  case b of
    Just (x' :>: _) -> pure ((name, x') : repls, xs')
    Nothing -> pure (repls, x : xs')
  where
    elemAs :: Assumption PlumeType -> [Assumption PlumeType] -> IO (Maybe (Assumption PlumeType))
    elemAs (_ :>: b1) =
      flip
        findM
        ( \(_ :>: b2) -> do
            b1' <- compressPaths b1
            b2' <- compressPaths b2

            doesUnifyWith b1' b2'
        )

findM :: (Monad m) => [a] -> (a -> m Bool) -> m (Maybe a)
findM (x : xs) f = do
  r <- f x
  if r
    then return (Just x)
    else findM xs f
findM [] _ = return Nothing

findClass :: (MonadChecker m) => Text -> m Class
findClass name = do
  MkClassEnv cenv <- gets (classEnv . environment)

  case Map.lookup name cenv of
    Just cls -> pure cls
    Nothing -> throw $ UnboundVariable name

instantiateQual :: (MonadChecker m) => Substitution -> Qualified PlumeQualifier -> m (Qualified PlumeQualifier, Substitution)
instantiateQual s (ps :=>: h) = do
  (ps', s1) <-
    Fold.foldrM
      ( \p (acc, sAcc) -> do
          (p', s') <- instantiateTyQual sAcc p
          pure (p' : acc, s')
      )
      ([], s)
      ps
      
  (h', s2) <- instantiateTyQual s1 h
  pure (ps' :=>: h', s2)

instantiateTyQual :: (MonadChecker m) => Substitution -> PlumeQualifier -> m (PlumeQualifier, Substitution)
instantiateTyQual s (IsIn ty name) = do
  (ty', _, s') <- instantiateWithSub s (Forall [] $ [] :=>: ty)
  pure (IsIn ty' name, s')
instantiateTyQual s (IsQVar name) = pure (IsQVar name, s)

instantiateClass :: (MonadChecker m) => Class -> m Class
instantiateClass (MkClass qvars quals methods) = do
  sub <- Map.fromList <$> mapM (\c -> (c,) <$> fresh) qvars
  (quals', s) <- instantiateQual sub quals
  methods' <- mapM (instantiateWithSub s) methods

  let methods'' = fmap (\(t, ps, _) -> Forall [] (ps :=>: t)) methods'

  pure (MkClass qvars quals' methods'')

unifiyTyQualWith :: (MonadChecker m) => PlumeQualifier -> PlumeQualifier -> m ()
unifiyTyQualWith (IsIn ty1 name1) (IsIn ty2 name2) | name1 == name2 = ty1 `unifiesWith` ty2
unifiyTyQualWith _ _ = throw $ CompilerError "Mismatched typeclasses"

unifyQualWith :: (MonadChecker m) => Qualified PlumeQualifier -> Qualified PlumeQualifier -> m ()
unifyQualWith (ps1 :=>: h1) (ps2 :=>: h2) = do
  zipWithM_ unifiyTyQualWith ps1 ps2
  h1 `unifiyTyQualWith` h2

createInstName :: PlumeType -> Text
createInstName (TypeId name) = name
createInstName (TypeApp x xs) = createInstName x <> "_" <> buildArray xs
  where
    buildArray [t] = createInstName t
    buildArray (t : ts) = createInstName t <> "_" <> buildArray ts
    buildArray [] = ""
createInstName (TypeVar _) = "tvar"
createInstName (TypeQuantified _) = "tvar"

removeSuperclasses :: MonadIO m => [PlumeQualifier] -> [PlumeQualifier] -> m [PlumeQualifier]
removeSuperclasses [] _ = pure []
removeSuperclasses (x : xs) ys = do
  found <- findMatchingClass x ys
  case found of
    [] -> (x :) <$> removeSuperclasses xs ys
    _  -> removeSuperclasses xs (ys List.\\ found)

findMatchingClass :: MonadIO m => PlumeQualifier -> [PlumeQualifier] -> m [PlumeQualifier]
findMatchingClass (IsIn t1 n1) qs = flip filterM qs $ \case
  IsIn t2 n2 -> do
    b <- liftIO $ doesUnifyWith t1 t2
    pure $ b && n1 == n2
  _ -> pure False
findMatchingClass _ _ = pure []

instantiateFromName :: (MonadChecker m) => Text -> PlumeScheme -> m (PlumeType, [PlumeQualifier], Placeholder Post.Expression)
instantiateFromName name sch = do
  (ty, qs) <- instantiate sch
  let r = liftPlaceholders name ty qs
  pure (ty, qs, r)

isInSuperclassOf :: MonadChecker m => PlumeQualifier -> [PlumeQualifier] -> m Bool
isInSuperclassOf p@(IsIn t n) ps = not . null <$> filterM (\case
    IsIn t' n' -> do
      MkClass _ (quals :=>: _) _ <- findClass n'
      if null quals then do
        _t <- liftIO $ compressPaths t
        _t' <- liftIO $ compressPaths t'
        if _t /= _t' then do
          bl <- liftIO $ doesUnifyWith _t _t'
          return (n == n' && bl)
        else pure False
      else isInSuperclassOf p quals
    _ -> pure False
  ) ps
isInSuperclassOf _ _ = pure False

removeSuperclassesQuals :: MonadChecker m => [PlumeQualifier] -> m [PlumeQualifier]
removeSuperclassesQuals [] = pure []
removeSuperclassesQuals [x] = pure [x]
removeSuperclassesQuals (x : xs) = do
  xs' <- removeSuperclassesQuals xs
  b <- isInSuperclassOf x xs'
  if b
    then pure xs'
    else pure (x : xs)

-- SOME INSTANCES FOR DISCHARGING
instance Semigroup PlumeQualifier where
  IsIn a b <> IsIn a' b' | b == b' = IsIn (a <> a') b
  _ <> _ = error "Mismatched typeclasses"

instance Semigroup PlumeType where
  TypeId a <> TypeId b | a == b = TypeId a
  TypeApp a b <> TypeApp a' b' | a == a' = TypeApp a (b <> b')
  TypeVar a <> TypeVar b | a == b = TypeVar a
  TypeQuantified a <> TypeQuantified b = TypeQuantified (a <> b)
  _ <> _ = error "Mismatched types"