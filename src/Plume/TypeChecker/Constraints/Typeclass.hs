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
import Data.Tuple.Utils (thd3)
import qualified Plume.TypeChecker.Monad as M

findMatchingFunDep :: (MonadChecker m) => PlumeType -> m (Maybe (PlumeType, (Text, PlumeType)))
findMatchingFunDep t1 = do
  funDeps <- gets (funDeps . environment)
  t1' <- liftIO $ compressPaths t1

  findM (Map.toList funDeps) $ \(t2, _) -> do
    t2' <- liftIO $ compressPaths t2
    liftIO $ doesUnifyWith' t1' t2'

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
  s <- gets substitution
  -- Checking if some extension exists for the given qualifier and getting the
  -- first to match.
  p' <- liftIO $ applyQual s =<< compressQual p
  x <- forM (getQuals cenv) $ \(qvs, sch) -> do
    sub <- Map.fromList <$> mapM (\c -> (c,) <$> fresh) qvs
    (a :=>: b, _) <- instantiateQual sub sch
    b' <- liftIO $ compressQual b

    First <$> (fmap (a,b,) <$> matchMut' b' p') `tryOr` pure Nothing

  case getFirst $ mconcat x of
    Just (_ps, b, _) -> do
      _ps' <- liftIO $ mapM (applyQual s) _ps
      -- Removing already deduced superclasses, e.g. boolean_algebra A is a
      -- superclass of equality A, so if both are presents, we can remove 
      -- boolean_algebra qualifier.
      let ps = List.nub $ removeQVars _ps'
      _ps'' <- removeSuperclassesQuals ps

      -- Recursively discharging environment in order to get smaller pieces of
      -- qualifiers
      (ps', mp, as, ds) <-
        fmap mconcat
          . mapM (discharge cenv)
          $ _ps''

      -- Getting dictionary stuffs for generating calls, map, assumptions..
      let ty = getDictTypeForPred p'
      t <- liftIO $ compressPaths ty

      let d = Post.EVariable (Cmm.fromText (getDict b)) (Identity t)
          e = if null ds then d else Post.EApplication d ds

      newSub <- gets substitution
      ps'' <- liftIO $ mapM (applyQual newSub) ps'
      p'' <- liftIO $ applyQual newSub p'

      pure (ps'', mp <> [(p'', e)], as, pure e)
    Nothing -> do
      -- Checking for potential matching function dependencies
      case p' of
        IsIn xs'@(x':y:_) n -> do
          matching <- findMatchingFunDep x'
          
          case matching of
            Just (ty, (n', y')) | n == n' -> do
              -- First we resolve general extension type with the given type
              s1 <- unifyAndGetSub x' ty
              -- Then we resolve inner type with the given type
              s2 <- unifyAndGetSub y y'

              -- We compose to get the final substitution for th given type
              -- This is used to apply the general substitution to the inner 
              -- type.
              s'' <- liftIO $ compose s1 s2

              xs'' <- liftIO $ mapM (apply s'') xs'

              -- Saving locally the new substitution
              M.modify $ \st -> st {substitution = s'' <> st.substitution}

              -- Recursively discharging environment in order to get smaller pieces of
              -- qualifiers for the new type
              if null s'' 
                then dischargeCallback p' p
                else discharge cenv (IsIn xs'' n)
            _ -> dischargeCallback p' p
        _ -> dischargeCallback p' p

dischargeCallback ::
  (MonadChecker m) =>
  PlumeQualifier ->
  PlumeQualifier ->
  m
    ( [PlumeQualifier],
      [(PlumeQualifier, Post.Expression)],
      [Assumption PlumeType],
      [Post.Expression]
    )
dischargeCallback p' p = do
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
getDictTypeForPred (IsIn c t) = TypeApp (getDictName t) c
getDictTypeForPred (IsQVar t) = TypeQuantified t

getDict :: PlumeQualifier -> Text
getDict (IsIn c t) = t <> "_" <> createInstNames c
getDict (IsQVar t) = t

createInstNames :: [PlumeType] -> Text
createInstNames = List.foldl' (\acc x -> acc <> "_" <> createInstName x) ""

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
        (IsIn c t') -> IsIn (map normtype c) t'
        _ -> error "Impossible") xs :=>: normtype zs

    normtype :: PlumeType -> PlumeType
    normtype (TypeId a) = TypeId a
    normtype (TypeApp a b) = TypeApp (normtype a) (map normtype b)
    normtype (TypeVar c) = TypeVar c
    normtype (TypeQuantified a) = TypeQuantified a

-- SOME UNIFYING FUNCTIONS FOR DISCHARGING
doesMatch :: (MonadChecker m) => PlumeType -> PlumeType -> m Bool
doesMatch (TypeId "variable") (TypeId "list") = pure True
doesMatch (TypeId "list") (TypeId "variable") = pure True
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
  a1 <- liftIO $ mapM compressPaths a
  a2 <- liftIO $ mapM compressPaths a'
  bl <- and <$> zipWithM doesMatch a1 a2
  pure $ bl && b == b'
doesMatchQual _ _ = pure False

matchMut :: (MonadChecker m) => PlumeType -> PlumeType -> m ()
matchMut (TypeId "variable") (TypeId "list") = pure ()
matchMut (TypeId "list") (TypeId "variable") = pure ()
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
  a1' <- liftIO $ mapM compressPaths a1
  a2' <- liftIO $ mapM compressPaths a2
  Just <$> zipWithM_ matchMut a1' a2'
matchMut' _ _ = pure Nothing

matchMut'' :: (MonadChecker m) => PlumeType -> PlumeType -> m ()
matchMut'' t1 t2 = do
  t1' <- liftIO $ compressPaths t1
  t2' <- liftIO $ compressPaths t2
  matchMut t1' t2'

-- SOME QUALIFIER FUNCTIONS

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
instantiateTyQual s (IsIn tys name) = do
  (tys', s') <- foldlM (\(ts, s') t -> do
      (t', _, s'') <- instantiateWithSub s' (Forall [] $ [] :=>: t)
      return (ts <> [t'], s'')
    ) ([], s) tys
  pure (IsIn tys' name, s')
instantiateTyQual s (IsQVar name) = pure (IsQVar name, s)

instantiateClass :: (MonadChecker m) => Class -> m Class
instantiateClass (MkClass qvars quals methods ded) = do
  sub <- Map.fromList <$> mapM (\c -> (c,) <$> fresh) qvars
  (quals', s) <- instantiateQual sub quals
  methods' <- mapM (instantiateWithSub s) methods

  s' <- liftIO $ composeSubs (map thd3 (Map.elems methods'))

  ded' <- case ded of
    Just (x, y) -> liftIO $ Just <$> do
      x' <- apply s' x
      y' <- apply s' y
      return (x', y')
    Nothing -> pure Nothing

  let methods'' = fmap (\(t, ps, _) -> Forall [] (ps :=>: t)) methods'

  pure (MkClass qvars quals' methods'' ded')

unifiyTyQualWith :: (MonadChecker m) => PlumeQualifier -> PlumeQualifier -> m ()
unifiyTyQualWith (IsIn ty1 name1) (IsIn ty2 name2) | name1 == name2 = 
  zipWithM_ (\t1 t2 -> do
    t1' <- liftIO $ compressPaths t1
    t2' <- liftIO $ compressPaths t2
    b <- liftIO $ doesUnifyWith t1' t2'

    when b $ t1' `unifiesWith` t2') ty1 ty2
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
    b <- liftIO $ and <$> zipWithM doesUnifyWith t1 t2
    pure $ b && n1 == n2
  _ -> pure False
findMatchingClass _ _ = pure []

instantiateFromName :: (MonadChecker m) => Text -> PlumeScheme -> m (PlumeType, [PlumeQualifier], Placeholder Post.Expression, Bool)
instantiateFromName name sch = do
  (ty, qs) <- instantiate sch
  let r = liftPlaceholders name ty qs
  pure (ty, qs, r, False)

isInSuperclassOf :: MonadChecker m => PlumeQualifier -> [PlumeQualifier] -> m Bool
isInSuperclassOf p@(IsIn t n) ps = not . null <$> filterM (\case
    IsIn t' n' -> do
      MkClass _ (quals :=>: _) _ _ <- findClass n'
      if null quals then do
        _t <- liftIO $ mapM compressPaths t
        _t' <- liftIO $ mapM compressPaths t'
        if _t /= _t' then do
          bl <- liftIO $ and <$> zipWithM doesUnifyWith _t _t'
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