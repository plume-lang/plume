{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Plume.TypeChecker.Checker where

import Control.Monad.Except
import Data.Foldable
import Data.List qualified as L
import Data.Map qualified as M
import Data.Set qualified as S
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Literal
import Plume.Syntax.Common.Pattern qualified as Pre
import Plume.Syntax.Common.Type qualified as Pre
import Plume.Syntax.Concrete (Position)
import Plume.Syntax.Concrete.Expression qualified as Post (TypeConstructor (..))
import Plume.Syntax.Translation.Generics qualified as G
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Constraints.Solver qualified as Solver
import Plume.TypeChecker.Monad
import Plume.TypeChecker.Monad.Type qualified as Post
import Plume.TypeChecker.Monad.Type.Conversion
import Plume.TypeChecker.TLIR qualified as Post
import Prelude hiding (gets, local, (++))
import Prelude qualified as P

type Expression = Post.TypedExpression Post.PlumeType
type Pattern = Post.TypedPattern Post.PlumeType

(++) :: (Eq a) => [a] -> [a] -> [a]
(++) xs ys = L.nub $ xs P.++ ys

generateQualified :: [PlumeGeneric] -> [Qualified]
generateQualified =
  concat
    . mapMaybe
      ( \case
          GVar _ -> Nothing
          GExtends n tys -> Just (map (TVar n `Has`) tys)
      )

unify :: (MonadChecker m) => ConstraintConstructor -> m ()
unify c = do
  st <- readIORef checkerST
  case st.position of
    Just p -> do
      writeIORef
        checkerST
        ( st
            { constraints =
                st.constraints {typesConstr = (p, c) : typesConstr st.constraints}
            }
        )
    Nothing -> throw (CompilerError "No position found")

unifyExt :: (MonadChecker m) => ConstraintConstructor -> m ()
unifyExt c = do
  st <- readIORef checkerST
  case st.position of
    Just p -> do
      writeIORef
        checkerST
        ( st
            { constraints =
                st.constraints {extensionsConstr = (p, c) : extensionsConstr st.constraints}
            }
        )
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
      (instantiated, _, qs) <- instantiate t'
      return (Just (instantiated, qs))
    Nothing -> do
      cons <- search @"types" name
      case cons of
        Just t' -> do
          (instantiated, _, qs) <- instantiate t'
          return (Just (instantiated, qs))
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

getConstraints
  :: (MonadChecker m) => m a -> m (a, ([TypeConstraint], [TypeConstraint]))
getConstraints m = do
  old <- readIORef checkerST
  a <- m
  s <- readIORef checkerST
  let tc =
        removeNewEntries s.constraints.typesConstr old.constraints.typesConstr
      ec =
        removeNewEntries s.constraints.extensionsConstr old.constraints.extensionsConstr
  writeIORef
    checkerST
    ( old
        { constraints = Constraints tc ec
        , tvarCounter = s.tvarCounter
        }
    )
  let tcDiff = s.constraints.typesConstr L.\\ old.constraints.typesConstr
  let ecDiff = s.constraints.extensionsConstr L.\\ old.constraints.extensionsConstr
  return (a, (tcDiff, ecDiff))

removeNewEntries :: (Eq a) => [a] -> [a] -> [a]
removeNewEntries xs ys = xs L.\\ (xs L.\\ ys)

synthesize' :: Inference m Pre.Expression Expression
synthesize' (Pre.EVariable name) = do
  t <- search @"variables" name
  case t of
    Just t' -> do
      (inst, _, qs) <- instantiate t'
      return (inst, G.Single $ Post.EVariable name inst, qs)
    Nothing -> do
      cons <- search @"types" name
      case cons of
        Just t' -> do
          (inst, _, qs) <- instantiate t'
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
          case (ts, xs') of
            ([], []) -> throw (UnboundVariable n)
            (t : ts', extVar : restArgs) -> do
              ret <- freshTVar
              let extTy = [t] :->: (ts' :->: ret)

              unify (Extends t n extTy)
              return
                ( ret
                , G.Single $
                    Post.EApplication
                      (Post.EApplication (Post.EExtVariable n extTy) [extVar])
                      restArgs
                , (t `Has` n) : concat qs'
                )
            _ -> throw (CompilerError "Invalid application")
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
    ( args' :->: ret'
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
  let none x = (x, ([], []))
  ((t, value', qs'), (tcs, ecs)) <-
    (if not (null genericTys) then getConstraints else (none <$>)) $
      withVariables [(name, Forall [] (qs :=>: ty'))] $
        withGenerics generics'' $
          synthesize value

  unify (ty' :~: t)

  (sch, sub) <-
    if null genericTys
      then return (Forall [] ((qs <> qs') :=>: ty'), mempty)
      else do
        s1 <- Solver.solve tcs
        s2 <- Solver.solve (map (second $ apply s1) ecs)

        let sub = compose s2 s1
        let appliedGens = apply sub genericTys
        let appliedTy = apply sub t
        let appliedQs = apply sub (qs <> qs')
        modifyIORef'
          checkerST
          (\s -> s {variables = apply sub s.variables})
        return (Forall (map extract appliedGens) (appliedQs :=>: appliedTy), sub)

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
        , L.nub qs''
        )
    Nothing -> do
      insert @"variables" name sch
      return
        ( TUnit
        , G.Single . apply sub $
            Post.EDeclaration
              (Annotation name t)
              genericTys
              value'
              Nothing
        , []
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
  let schemes' = map (\(name, s) -> (Extension name t' [], s)) schemes
  mapM_ (uncurry $ insert @"extensions") schemes'
  return (Post.TUnit, G.Spread members', [])
synthesize' (Pre.ENativeFunction filepath name gens ty) = do
  gens' <- mapM (const fresh) gens
  let genericList = zip gens gens'
  t <- withGenerics genericList $ convert ty
  insert @"variables" name (Forall gens' ([] :=>: t))
  return (t, G.Single $ Post.ENativeFunction filepath name (map GVar gens') t, [])
synthesize' (Pre.EGenericProperty {-gens name tys ty-} {}) = do
  throw (CompilerError "Generic properties are no longer supported")
-- gens' <- mapM convert gens
-- let qs = generateQualified gens'
-- let genericList = zip (map getGenericName gens) (fmap extract gens')

-- (tys', ty') <- withGenerics genericList $ do
--   tys' <- mapM convert tys
--   ty' <- withGenerics genericList $ convert ty
--   return (tys', ty')

-- case tys' of
--   [] -> throw (TypeMissing name)
--   (extTy : rest) -> do
--     let funTy = [extTy] :->: (rest :->: ty')
--     let sch = Forall (map extract gens') (qs :=>: funTy)

--     return
--       ( ty'
--       , G.Empty
--       , []
--       )
synthesize' (Pre.EList es) = do
  (ts, es', qs) <- mapAndUnzip3M synthesize es
  t <- freshTVar
  forM_ ts $ unify . (t :~:)
  return (Post.TList t, G.Single $ Post.EList es', concat qs)
synthesize' (Pre.EType (Annotation name gens) ts) = do
  gens' <- mapM convert gens
  let gens'' = map extract gens'
  let tyGens = map TVar gens''
  ts' <- mapM convert ts

  let constructors =
        map
          ( \(cname, tys) -> case tys of
              [] -> Post.TVariable cname
              _ -> Post.TConstructor cname tys
          )
          ts'

  let headerTy =
        if null gens'
          then Post.TId name
          else Post.TApp (Post.TId name) tyGens
  let schs =
        map
          ( \(name', ty) ->
              ( name'
              , Forall
                  gens''
                  ([] :=>: (ty :->: headerTy))
              )
          )
          ts'
  mapM_ (uncurry $ insert @"types") schs
  return
    (Post.TUnit, G.Single (Post.EType (Annotation name gens') constructors), [])

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
          (\n -> (n,) . Forall [] . ([] :=>:))
          (map annotationName args)
          args'

  let none x = (x, ([], []))
  ((t, body', qs'), (tcs, ecs)) <-
    (if not (null genericTys) then getConstraints else (none <$>)) $
      withGenerics generics'' $
        withVariables args'' $
          withReturnType ret' $
            synthesize e

  unify (ret' :~: t)

  let funTy = [snd var] :->: (args' :->: ret')

  let finalQs = qs ++ qs'

  (sch@(Forall _ (_ :=>: newFunTy)), sub) <-
    if null genericTys
      then return (Forall (map extract extGens) ((qs <> qs') :=>: funTy), mempty)
      else do
        s1 <- Solver.solve tcs
        s2 <- Solver.solve (map (second $ apply s1) ecs)
        let sub = compose s2 s1

        let appliedGens = apply sub (extGens ++ genericTys)
        let appliedTy = apply sub funTy
        let appliedQs = apply sub finalQs
        modifyIORef'
          checkerST
          (\s -> s {variables = apply sub s.variables})
        return (Forall (map extract appliedGens) (appliedQs :=>: appliedTy), sub)

  let appliedNewGens = apply sub (extGens ++ genericTys)
  let args''' =
        zipWith
          Annotation
          (map annotationName args)
          ( case newFunTy of
              (_ :->: (newFunArgs :->: _)) -> newFunArgs
              _ -> args'
          )

  return
    ( Post.EExtensionDeclaration
        name
        (snd var)
        appliedNewGens
        (uncurry Annotation var)
        (Post.EClosure args''' ret' body')
    , (name, sch)
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
      return (inst, Post.PSpecialVar name inst, mempty)
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
mapAndUnzip3M f xs = do
  res <- mapM f xs
  let (bs, cs, ds) = unzip3 res
  return (bs, cs, ds)

runSynthesize
  :: (MonadIO m)
  => [Pre.Expression]
  -> m (Either (TypeError, Maybe Position) [Expression])
runSynthesize e = do
  runExceptT
    ( do
        exprs <- mapM synthesize' e
        let (_, es, _) = unzip3 exprs
        let es' = G.flat es

        cs <- gets constraints
        s1 <- Solver.solve cs.typesConstr
        s2 <- Solver.solve (map (second $ apply s1) cs.extensionsConstr)
        let sub = compose s2 s1

        let es'' = apply sub es'

        let freedTys = free es''

        unless (null freedTys) $
          throw (UnboundTypeVariable (S.findMin freedTys))

        return es''
    )