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

generateQualified :: [PlumeGeneric] -> [Qualified]
generateQualified =
  concat
    . mapMaybe
      ( \case
          GVar _ -> Nothing
          GExtends n tys -> Just (map (TVar n `Has`) tys)
      )

getGenericProperty
  :: (MonadChecker m) => Text -> m (Maybe (PlumeType, [Qualified]))
getGenericProperty name = do
  exts <- gets extensions
  let t = findWithKey (\(Extension n _ b _) -> n == name && b) exts
  case t of
    Just (_, sch) ->
      instantiate sch >>= \case
        (t', _, qs) -> return (Just (t', qs))
    _ -> return Nothing

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
  :: (MonadChecker m)
  => m (PlumeType, Result to, [Qualified])
  -> m (PlumeType, to, [Qualified])
shouldBeAlone m = do
  (t, xs, qs) <- m
  case xs of
    G.Single x -> return (t, x, qs)
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
  :: (MonadChecker m) => Text -> m (Maybe (PlumeType, [Qualified]))
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

checkExtensions :: (MonadChecker m) => Scheme -> m (PlumeType, [Qualified])
checkExtensions t'@(Forall _ _) = do
  (instantiated, _, qs) <- instantiate t'
  mapM_ (\(ty `Has` n) -> unify' (ExtensionExists n ty)) qs
  return (instantiated, qs)

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
      (inst, qs) <- checkExtensions t'
      return (inst, G.Single $ Post.EVariable name inst, qs)
    Nothing -> throw (UnboundVariable name)
synthesize' (Pre.EApplication f xs)
  | Just (Pre.EVariable n) <- spanProperty f = do
      ty <- getVariable n
      case ty of
        Just (t, qs) -> do
          (ts, xs', qs') <- mapAndUnzip3M synthesize xs
          ret <- freshTVar
          unify (t :~: ts :->: ret)
          return
            (ret, G.Single $ Post.EApplication (Post.EVariable n t) xs', qs ++ concat qs')
        _ -> do
          (ts, xs', qs') <- mapAndUnzip3M synthesize xs
          case ts of
            [] -> throw (UnboundVariable n)
            (t : ts') -> do
              ret <- freshTVar
              let extTy = (t : ts') :->: ret
              unify (Extends t n extTy)
              return
                ( ret
                , G.Single $ Post.EApplication (Post.EExtVariable n extTy) xs'
                , (t `Has` n) : concat qs'
                )
  | otherwise = do
      (t, f', qs) <- synthesize f
      (ts, xs', qs') <- mapAndUnzip3M synthesize xs
      ret <- freshTVar
      unify (t :~: ts :->: ret)
      return (ret, G.Single $ Post.EApplication f' xs', concat qs' ++ qs)
synthesize' (Pre.ELiteral l) = return $ case l of
  LInt _ -> (Post.TInt, G.Single $ Post.ELiteral l, [])
  LBool _ -> (Post.TBool, G.Single $ Post.ELiteral l, [])
  LString _ -> (Post.TString, G.Single $ Post.ELiteral l, [])
  LChar _ -> (Post.TChar, G.Single $ Post.ELiteral l, [])
  LFloat _ -> (Post.TFloat, G.Single $ Post.ELiteral l, [])
synthesize' (Pre.EClosure args ret body) = do
  ret' <- fromType ret
  args' <- mapM (fromType . annotationValue) args
  let args'' =
        zipWith
          (\n -> (n,) . Forall [] . ([] :=>:))
          (map annotationName args)
          args'
  (t, body', qs) <-
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
    , qs
    )
synthesize' (Pre.EConditionBranch cond then' else') = case else' of
  Just else'' -> do
    (t, cond', qs1) <- synthesize cond
    (t', then'', qs2) <- synthesize then'
    (t'', else''', qs3) <- synthesize else''
    unify (t :~: Post.TBool)
    unify (t' :~: t'')
    unify (t' :~: t'')
    return
      ( t'
      , G.Single $ Post.EConditionBranch cond' then'' (Just else''')
      , qs1 ++ qs2 ++ qs3
      )
  Nothing -> do
    (t, cond', qs1) <- synthesize cond
    (t', then'', qs2) <- synthesize then'
    unify (t :~: Post.TBool)
    return (t', G.Single $ Post.EConditionBranch cond' then'' Nothing, qs1 ++ qs2)
synthesize' (Pre.EBlock es) = do
  (ret, (_, es', qss)) <- local id $ do
    es' <- mapAndUnzip3M (shouldBeAlone . synthesize') es
    ret <- gets returnType
    return (ret, es')
  return (ret, G.Single $ Post.EBlock es', concat qss)
synthesize' (Pre.ELocated e p) = do
  (t, e', qs) <- with @"position" (Just p) $ synthesize' e
  return (t, G.mapSpreadable (`Post.ELocated` p) e', qs)
synthesize' (Pre.EReturn e) = do
  (t, e', qs) <- synthesize e
  ret <- gets returnType
  unify (t :~: ret)
  return (ret, G.Single $ Post.EReturn e', qs)
synthesize' (Pre.EDeclaration gens (Annotation name ty) value body) = do
  (genericTys, genericNames) <-
    (,map getGenericName gens) <$> mapM convert gens

  let qs = generateQualified genericTys

  let generics'' = zip genericNames (map extract genericTys)
  ty' <- fromType ty
  (t, value', qs') <-
    withVariables [(name, Forall [] (qs :=>: ty'))] $
      withGenerics generics'' $
        synthesize value
  unify (ty' :~: t)
  let sch =
        if null genericTys
          then Forall [] (qs' :=>: ty')
          else Forall (map extract genericTys) (qs :=>: t)
  case body of
    Just body' -> do
      (ret, body'', qs'') <-
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
        , if null genericTys then qs' ++ qs'' else qs
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
        , if null genericTys then qs' else qs
        )
synthesize' (Pre.ESwitch e cases) = do
  (t, e', qs) <- synthesize e
  (cases', ts) <-
    mapAndUnzipM
      ( \(p, e'') -> do
          (patternTy, p', env) <- synthesizePat p
          (exprTy, e''', qs') <- withVariables (M.toList env) $ synthesize e''
          unify (t :~: patternTy)
          return ((p', e'''), (exprTy, qs'))
      )
      cases

  let (ts', qs') = unzip ts

  (ret, xs) <- case ts' of
    [] -> throw EmptyMatch
    (ret : xs) -> return (ret, xs)

  forM_ xs $ unify . (ret :~:)

  return (ret, G.Single $ Post.ESwitch e' cases', concat qs' ++ qs)
synthesize' (Pre.ETypeExtension generics (Annotation x t) members) = do
  generics' <- mapM convert generics
  let gensInt = zipWith (curry $ bimap getGenericName extract) generics generics'
  let qs = generateQualified generics'
  (t', (members', schemes)) <- withGenerics gensInt $ do
    t' <- convert t
    res <-
      unzip
        <$> withVariables
          [(x, Forall [] (qs :=>: t'))]
          ( do
              mapM
                ( \em -> do
                    synthesizeExtMember em (x, t', generics')
                )
                members
          )
    return (t', res)
  let superExtensions =
        concat $
          mapMaybe
            ( \case
                GExtends n tys -> Just $ map (\ty -> Extension ty (TVar n) False []) tys
                _ -> Nothing
            )
            generics'
  let schemes' = map (\(name, s) -> (Extension name t' False superExtensions, s)) schemes
  mapM_ (uncurry $ insert @"extensions") schemes'
  return (Post.TUnit, G.Spread members', [])
synthesize' (Pre.ENativeFunction name gens ty) = do
  gens' <- mapM (const fresh) gens
  let genericList = zip gens gens'
  t <- withGenerics genericList $ convert ty
  insert @"variables" name (Forall gens' ([] :=>: t))
  return (t, G.Single $ Post.ENativeFunction name (map GVar gens') t, [])
synthesize' (Pre.EGenericProperty gens name tys ty) = do
  gens' <- mapM convert gens
  let qs = generateQualified gens'
  let genericList = zip (map getGenericName gens) (fmap extract gens')

  (tys', ty') <- withGenerics genericList $ do
    tys' <- mapM convert tys
    ty' <- withGenerics genericList $ convert ty
    return (tys', ty')

  case tys' of
    [] -> throw (TypeMissing name)
    (extTy : _) -> do
      let funTy = tys' :->: ty'
      let sch = Forall (map extract gens') (qs :=>: funTy)
      let superExtensions =
            concat $
              mapMaybe
                ( \case
                    GExtends n exts -> Just $ map (\ext -> Extension ext (TVar n) False []) exts
                    _ -> Nothing
                )
                gens'

      insert @"extensions" (Extension name extTy True superExtensions) sch
      return
        ( ty'
        , G.Empty
        , []
        )
synthesize' (Pre.EList es) = do
  (ts, es', qs) <- mapAndUnzip3M synthesize es
  t <- freshTVar
  forM_ ts $ unify . (t :~:)
  return (Post.TList t, G.Single $ Post.EList es', concat qs)

synthesizeExtMember
  :: (MonadChecker m)
  => Pre.ExtensionMember Pre.PlumeType
  -> (Text, PlumeType, [PlumeGeneric])
  -> m (Expression, (Text, Scheme))
synthesizeExtMember (Pre.ExtDeclaration gens (Annotation name _) (Pre.EClosure args ret e)) (extName, extTy, extGens) = do
  let var = (extName, extTy)
  (genericTys, genericNames) <- (,map getGenericName gens) <$> mapM convert gens

  let qsTys = generateQualified genericTys
  let qsExt = generateQualified extGens
  let qs = qsTys ++ qsExt

  let generics'' = zipWith (curry $ second extract) genericNames genericTys

  ret' <- fromType ret
  args' <- mapM (fromType . annotationValue) args
  let args'' =
        zipWith
          (\n -> (n,) . Forall [] . (qs :=>:))
          (map annotationName args)
          args'

  (t, body', qs') <-
    withGenerics generics'' $
      withVariables args'' $
        withReturnType ret' $
          synthesize e

  unify (t :~: ret')

  let args''' = zipWith Annotation (map annotationName args) args'

  let funTy = (snd var : args') :->: ret'

  genProp <- getGenericProperty name

  let finalQs = qs <> qs'

  case genProp of
    Just (t', _) -> do
      unify (t' :~: funTy)
    Nothing -> return ()

  return
    ( Post.EDeclaration
        (Annotation name funTy)
        (extGens ++ genericTys)
        (Post.EClosure (uncurry Annotation var : args''') ret' body')
        Nothing
    , (name, Forall (map extract $ extGens ++ genericTys) (finalQs :=>: funTy))
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
      (inst, _, _) <- instantiate t'
      return (inst, Post.PVariable name inst, mempty)
    Nothing -> do
      ty <- freshTVar
      return (ty, Post.PVariable name ty, M.singleton name (Forall [] ([] :=>: ty)))
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
      (inst, _, _) <- instantiate t
      (ts, pats', vars) <- mapAndUnzip3M synthesizePat pats
      unify (inst :~: ts :->: tv)
      return (tv, Post.PConstructor name pats', mconcat vars)
    Nothing -> throw (UnboundVariable name)

synthesize
  :: (MonadChecker m) => Pre.Expression -> m (PlumeType, Expression, [Qualified])
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
      let (_, xs', _) = unzip3 xs
      let exprs = G.flat xs'
      return $ apply <$> sub <*> pure exprs
