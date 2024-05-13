{-# LANGUAGE LambdaCase #-}
module Plume.TypeChecker.Constraints.Typeclass where

import Data.List qualified as List
import Data.Map qualified as Map
import Plume.Compiler.Desugaring.Monad (freshName)
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Solver (unifiesWith)
import Plume.TypeChecker.Constraints.Unification (compressPaths, doesUnifyWith, compressQual)
import Plume.TypeChecker.TLIR qualified as Post
import Prelude hiding (gets)

instance Semigroup PlumeQualifier where
  IsIn a b <> IsIn a' b' | b == b' = IsIn (a <> a') b
  _ <> _ = error "Mismatched typeclasses"

instance Semigroup PlumeType where
  TypeId a <> TypeId b | a == b = TypeId a
  TypeApp a b <> TypeApp a' b' | a == a' = TypeApp a (b <> b')
  TypeVar a <> TypeVar b | a == b = TypeVar a
  TypeQuantified a <> TypeQuantified b = TypeQuantified (a <> b)
  _ <> _ = error "Mismatched types"

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
  p' <- liftIO $ compressQual p
  x <- forM (getQuals cenv) $ \(qvs, sch) -> do
    sub <- Map.fromList <$> mapM (\c -> (c,) <$> fresh) qvs
    (a :=>: b, _) <- instantiateQual sub sch
    b' <- liftIO $ compressQual b
    (fmap (a,b,) <$> matchMut' b' p') `tryOr` pure Nothing

  case mconcat x of
    Just (_ps, b, _) -> do
      let ps = removeQVars _ps
      (ps', mp, as, ds) <-
        fmap mconcat
          . traverse (discharge cenv)
          $ ps

      let t = getDictTypeForPred p'
          d = Post.EVariable (getDict b) t
          e = if null ds then d else Post.EApplication d ds
      pure (ps', mp <> [(p', e)], as, pure e)
    Nothing -> do
      param <- freshName
      let paramTy = getDictTypeForPred p'
      pure
        ( pure p,
          List.singleton (p', Post.EVariable param paramTy),
          pure $ param :>: paramTy,
          pure $ Post.EVariable param paramTy
        )

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

removeDuplicatesQuals :: (MonadChecker m) => [PlumeQualifier] -> m [PlumeQualifier]
removeDuplicatesQuals [] = pure []
removeDuplicatesQuals (x : xs) = do
  xs' <- removeDuplicatesQuals xs
  b <- liftIO $ elemQual x xs'
  if b
    then pure xs'
    else pure (x : xs')

elemQual :: PlumeQualifier -> [PlumeQualifier] -> IO Bool
elemQual (IsIn a1 b) =
  anyM
    ( \case
      (IsIn a2 b') -> do
        a1' <- compressPaths a1
        a2' <- compressPaths a2

        bl <- doesUnifyWith a1' a2'

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
    foldlM
      ( \(acc, sAcc) p -> do
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
