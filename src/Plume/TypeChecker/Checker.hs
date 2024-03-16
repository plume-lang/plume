{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Plume.TypeChecker.Checker where

import Control.Monad.Except
import Data.Foldable
import Data.Map qualified as M
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Literal
import Plume.Syntax.Common.Pattern qualified as Pre
import Plume.Syntax.Common.Type qualified as Pre
import Plume.Syntax.Concrete (Position)
import Plume.Syntax.Translation.Generics qualified as G
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Constraints.Solver qualified as Solver
import Plume.TypeChecker.Monad
import Plume.TypeChecker.Monad.Type qualified as Post
import Plume.TypeChecker.Monad.Type.Conversion
import Plume.TypeChecker.TLIR qualified as Post
import Prelude hiding (gets, local)

type Expression = Post.TypedExpression Post.PlumeType
type Pattern = Post.TypedPattern Post.PlumeType

unify :: (MonadChecker m) => ConstraintConstructor -> m ()
unify c = do
  pos <- position <$> readIORef checkerST
  case pos of
    Just p -> do
      s <- readIORef checkerST
      writeIORef checkerST (s {constraints = s.constraints ++ [(p, c)]})
    Nothing -> throw (CompilerError "No position found")

unify' :: (MonadChecker m) => ConstraintConstructor -> m ()
unify' c = do
  s <- readIORef checkerST
  case s.position of
    Just p ->
      writeIORef
        checkerST
        (s {extensionConstraints = s.extensionConstraints ++ [(p, c)]})
    Nothing -> throw (CompilerError "No position found")

shouldBeAlone
  :: (MonadChecker m) => m (PlumeType, Result to) -> m (PlumeType, to)
shouldBeAlone m = do
  (t, xs) <- m
  case xs of
    G.Single x -> return (t, x)
    _ -> throw (CompilerError "Expected single expression")

createAnnotation :: Text -> (a -> b) -> a -> Annotation b
createAnnotation name f = (name :@:) . f

fromType :: (MonadChecker m) => Maybe Pre.PlumeType -> m PlumeType
fromType = maybe freshTVar convert

spanProperty
  :: Pre.Expression -> Maybe Pre.Expression
spanProperty n@(Pre.EVariable _) = Just n
spanProperty (Pre.ELocated e _) = spanProperty e
spanProperty _ = Nothing

isExtension :: (MonadChecker m) => Text -> m Bool
isExtension name = do
  exts <- gets extensions
  case lookupWith exts (\n -> n.name == name) of
    Just _ -> return True
    Nothing -> return False

findMin :: (Ord k) => Map k v -> Maybe k
findMin m = case M.keys m of
  [] -> Nothing
  xs -> Just $ minimum xs

lookupWith :: (Eq k, Ord k) => Map k v -> (k -> Bool) -> Maybe v
lookupWith m f = do
  let newMap = M.filterWithKey (\k'' _ -> f k'') m
  findMin newMap >>= flip M.lookup newMap

getGenericName :: Pre.PlumeGeneric -> Text
getGenericName (Pre.GVar n) = n
getGenericName (Pre.GExtends n _) = n

getVariable
  :: (MonadChecker m) => Text -> m (Maybe PlumeType)
getVariable name = do
  t <- search @"variables" name
  case t of
    Just t' -> do
      instantiated <- checkExtensions t'
      return (Just instantiated)
    Nothing -> do
      isExtension name >>= \case
        True -> return Nothing
        False -> throw (UnboundVariable name)

getExtensions :: [PlumeGeneric] -> Map Int [Text]
getExtensions gens =
  M.fromList $
    mapMaybe
      (\case GVar _ -> Nothing; GExtends n tys -> Just (n, tys))
      gens

mapWithKeyM :: (Monad m, Ord k) => (k -> a -> m b) -> Map k a -> m (Map k b)
mapWithKeyM f m = M.fromList <$> mapM (\(k, v) -> (k,) <$> f k v) (M.toList m)

checkForInstance :: (MonadChecker m) => Map PlumeType [Text] -> m ()
checkForInstance m = do
  void $
    mapWithKeyM
      ( \k v -> do
          mapM_ (unify' . flip ExtensionExists k) v
      )
      m

checkExtensions :: (MonadChecker m) => Scheme -> m PlumeType
checkExtensions t'@(Forall gens _) = do
  (instantiated, s) <- instantiate t'
  let m = getExtensions gens
  let subExt =
        M.fromList . M.elems $
          M.mapMaybeWithKey
            (\k v -> case M.lookup k m of Just tys -> Just (v, tys); Nothing -> Nothing)
            s
  checkForInstance subExt
  return instantiated

applyOnGeneric :: Substitution -> PlumeGeneric -> PlumeGeneric
applyOnGeneric s (GVar n) = case M.lookup n s of
  Just (TVar t) -> GVar t
  _ -> GVar n
applyOnGeneric s (GExtends n tys) = case M.lookup n s of
  Just (TVar t) -> GExtends t tys
  _ -> GExtends n tys

synthesize' :: Inference m Pre.Expression Expression
synthesize' (Pre.EVariable name) = do
  t <- search @"variables" name
  case t of
    Just t' -> do
      inst <- checkExtensions t'
      return (inst, G.Single $ Post.EVariable name inst)
    Nothing -> throw (UnboundVariable name)
synthesize' (Pre.EApplication f xs)
  | Just (Pre.EVariable n) <- spanProperty f = do
      ty <- getVariable n
      case ty of
        Just t -> do
          (ts, xs') <- mapAndUnzipM synthesize xs
          ret <- freshTVar
          unify (t :~: ts :->: ret)
          return (ret, G.Single $ Post.EApplication (Post.EVariable n t) xs')
        _ -> do
          (ts, xs') <- mapAndUnzipM synthesize xs
          case ts of
            [] -> throw (UnboundVariable n)
            (t : ts') -> do
              ret <- freshTVar
              let extTy = (t : ts') :->: ret
              unify (Extends t n extTy)
              return (ret, G.Single $ Post.EApplication (Post.EExtVariable n extTy) xs')
  | otherwise = do
      (t, f') <- synthesize f
      (ts, xs') <- mapAndUnzipM synthesize xs
      ret <- freshTVar
      unify (t :~: ts :->: ret)
      return (ret, G.Single $ Post.EApplication f' xs')
synthesize' (Pre.ELiteral l) = return $ case l of
  LInt _ -> (Post.TInt, G.Single $ Post.ELiteral l)
  LBool _ -> (Post.TBool, G.Single $ Post.ELiteral l)
  LString _ -> (Post.TString, G.Single $ Post.ELiteral l)
  LChar _ -> (Post.TChar, G.Single $ Post.ELiteral l)
  LFloat _ -> (Post.TFloat, G.Single $ Post.ELiteral l)
synthesize' (Pre.EClosure args ret body) = do
  ret' <- fromType ret
  args' <- mapM (fromType . annotationValue) args
  let args'' =
        zipWith
          (\n -> (n,) . Forall [])
          (map annotationName args)
          args'
  (t, body') <-
    withVariables args'' $
      withReturnType ret' $
        synthesize body
  unify (t :~: ret')
  return
    ( args' :->: t
    , G.Single $
        Post.EClosure
          (zipWith Annotation (map annotationName args) args')
          ret'
          body'
    )
synthesize' (Pre.EConditionBranch cond then' else') = case else' of
  Just else'' -> do
    (t, cond') <- synthesize cond
    (t', then'') <- synthesize then'
    (t'', else''') <- synthesize else''
    unify (t :~: Post.TBool)
    unify (t' :~: t'')
    unify (t' :~: t'')
    return (t', G.Single $ Post.EConditionBranch cond' then'' (Just else'''))
  Nothing -> do
    (t, cond') <- synthesize cond
    (t', then'') <- synthesize then'
    unify (t :~: Post.TBool)
    return (t', G.Single $ Post.EConditionBranch cond' then'' Nothing)
synthesize' (Pre.EBlock es) = do
  (ret, (_, es')) <- local id $ do
    es' <- mapAndUnzipM (shouldBeAlone . synthesize') es
    ret <- gets returnType
    return (ret, es')
  return (ret, G.Single $ Post.EBlock es')
synthesize' (Pre.ELocated e p) = do
  (t, e') <- with @"position" (Just p) $ synthesize' e
  return (t, G.mapSpreadable (`Post.ELocated` p) e')
synthesize' (Pre.EReturn e) = do
  (t, e') <- synthesize e
  ret <- gets returnType
  unify (t :~: ret)
  return (ret, G.Single $ Post.EReturn e')
synthesize' (Pre.EDeclaration gens (Annotation name ty) value body) = do
  (genericTys, genericNames) <-
    (,map getGenericName gens) <$> mapM convert gens
  let generics'' = zip genericNames (map extract genericTys)
  ty' <- fromType ty
  (t, value') <-
    withVariables [(name, Forall [] ty')] $
      withGenerics generics'' $
        synthesize value
  unify (ty' :~: t)
  let sch = if null genericTys then Forall [] ty' else Forall genericTys t
  case body of
    Just body' -> do
      (ret, body'') <-
        withVariables [(name, sch)] $
          synthesize body'
      return
        ( ret
        , G.Single $
            Post.EDeclaration
              (Annotation name ty')
              genericTys
              value'
              (Just body'')
        )
    Nothing -> do
      insert @"variables" name sch
      return
        ( TUnit
        , G.Single $
            Post.EDeclaration
              (Annotation name ty')
              genericTys
              value'
              Nothing
        )
synthesize' (Pre.ESwitch e cases) = do
  (t, e') <- synthesize e
  (cases', ts) <-
    mapAndUnzipM
      ( \(p, e'') -> do
          (patternTy, p', env) <- synthesizePat p
          (exprTy, e''') <- withVariables (M.toList env) $ synthesize e''
          unify (t :~: patternTy)
          return ((p', e'''), exprTy)
      )
      cases
  (ret, xs) <- case ts of
    [] -> throw EmptyMatch
    (ret : xs) -> return (ret, xs)

  forM_ xs $ unify . (ret :~:)

  return (ret, G.Single $ Post.ESwitch e' cases')
synthesize' (Pre.ETypeExtension generics (Annotation x t) members) = do
  generics' :: [Post.PlumeGeneric] <- mapM convert generics
  let gensInt = zipWith (curry $ bimap getGenericName extract) generics generics'
  (t', (members', schemes)) <- withGenerics gensInt $ do
    t' <- convert t
    res <-
      unzip
        <$> withVariables
          [(x, Forall [] t')]
          (mapM (`synthesizeExtMember` (x, t', generics')) members)
    return (t', res)
  let schemes' = map (\(name, s) -> (Extension name t' False, s)) schemes
  mapM_ (uncurry $ insert @"extensions") schemes'
  return (Post.TUnit, G.Spread members')
synthesize' (Pre.ENativeFunction name gens ty) = do
  gens' <- mapM (const fresh) gens
  let genericList = zip gens gens'
  ty' <- withGenerics genericList $ convert ty
  insert @"variables" name (Forall (map GVar gens') ty')
  return (ty', G.Single $ Post.ENativeFunction name (map GVar gens') ty')
synthesize' (Pre.EGenericProperty gens name tys ty) = do
  gens' <- mapM convert gens
  let genericList = zip (map getGenericName gens) (fmap extract gens')

  (tys', ty') <- withGenerics genericList $ do
    tys' <- mapM (convert @Pre.PlumeType @Post.PlumeType) tys
    ty' <- withGenerics genericList $ convert ty
    return (tys', ty')

  case tys' of
    [] -> throw (TypeMissing name)
    (extTy : _) -> do
      let funTy = tys' :->: ty'
      let sch = Forall gens' funTy
      insert @"extensions" (Extension name extTy True) sch
      return
        ( ty'
        , G.Empty
        )

synthesizeExtMember
  :: (MonadChecker m)
  => Pre.ExtensionMember Pre.PlumeType
  -> (Text, PlumeType, [PlumeGeneric])
  -> m (Expression, (Text, Scheme))
synthesizeExtMember (Pre.ExtDeclaration gens (Annotation name _) (Pre.EClosure args ret e)) (extName, extTy, extGens) = do
  let var = (extName, extTy)
  (genericTys, genericNames) <- (,map getGenericName gens) <$> mapM convert gens
  let generics'' = zipWith (curry $ second extract) genericNames genericTys

  ret' <- fromType ret
  args' <- mapM (fromType . annotationValue) args
  let args'' =
        zipWith
          (\n -> (n,) . Forall [])
          (map annotationName args)
          args'

  (t, body') <-
    withGenerics generics'' $
      withVariables args'' $
        withReturnType ret' $
          synthesize e

  unify (t :~: ret')

  let args''' = zipWith Annotation (map annotationName args) args'

  let funTy = (snd var : args') :->: ret'

  return
    ( Post.EDeclaration
        (Annotation name funTy)
        genericTys
        (Post.EClosure (uncurry Annotation var : args''') ret' body')
        Nothing
    , (name, Forall (extGens ++ genericTys) funTy)
    )
synthesizeExtMember _ _ = throw (CompilerError "Invalid extension member")

synthesizePat
  :: (MonadChecker m)
  => Pre.Pattern
  -> m (Post.PlumeType, Pattern, Map Text Scheme)
synthesizePat (Pre.PVariable name) = do
  t <- search @"types" name
  case t of
    Just t' -> do
      inst <- fst <$> instantiate t'
      return (inst, Post.PVariable name inst, mempty)
    Nothing -> do
      ty <- freshTVar
      return (ty, Post.PVariable name ty, M.singleton name (Forall [] ty))
synthesizePat Pre.PWildcard = do
  ty <- freshTVar
  return (ty, Post.PWildcard, mempty)
synthesizePat (Pre.PLiteral l) = do
  ty <- case l of
    LInt _ -> return Post.TInt
    LBool _ -> return Post.TBool
    LString _ -> return Post.TString
    LChar _ -> return Post.TChar
    LFloat _ -> return Post.TFloat
  return (ty, Post.PLiteral l, mempty)
synthesizePat (Pre.PConstructor name pats) = do
  consTy <- search @"types" name
  tv <- freshTVar
  case consTy of
    Just t -> do
      inst <- fst <$> instantiate t
      (ts, pats', vars) <- mapAndUnzip3M synthesizePat pats
      unify (inst :~: ts :->: tv)
      return (tv, Post.PConstructor name pats', mconcat vars)
    Nothing -> throw (UnboundVariable name)

synthesize :: (MonadChecker m) => Pre.Expression -> m (PlumeType, Expression)
synthesize = shouldBeAlone . local id . synthesize'

mapAndUnzip3M :: (Monad m) => (a -> m (b, c, d)) -> [a] -> m ([b], [c], [d])
mapAndUnzip3M f =
  foldrM
    ( \x (bs, cs, ds) -> do
        (b, c, d) <- f x
        return (b : bs, c : cs, d : ds)
    )
    ([], [], [])

runSynthesize
  :: (MonadIO m)
  => [Pre.Expression]
  -> m (Either (TypeError, Maybe Position) [Expression])
runSynthesize e = do
  runExceptT (mapM synthesize' e) >>= \case
    Left err -> return (Left err)
    Right xs -> do
      cnsts <- gets constraints
      sub <- Solver.runSolver cnsts
      let exprs = G.flat $ map snd xs
      return $ apply <$> sub <*> pure exprs
