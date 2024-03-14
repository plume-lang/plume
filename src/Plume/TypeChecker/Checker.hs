{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Plume.TypeChecker.Checker where

import Control.Monad.Except
import Data.Foldable
import Data.Map qualified as M
import GHC.Records
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Literal
import Plume.Syntax.Common.Pattern qualified as Pre
import Plume.Syntax.Common.Type qualified as Pre
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Monad
import Plume.TypeChecker.Monad.Type qualified as Post
import Plume.TypeChecker.Monad.Type.Conversion
import Plume.TypeChecker.TLIR qualified as Post
import Prelude hiding (gets, local)

type Expression = Post.TypedExpression Post.PlumeType
type Pattern = Post.TypedPattern Post.PlumeType

throw :: (MonadChecker m) => TypeError -> m a
throw err =
  throwError . (err,) =<< position <$> readIORef checkerST

unify :: (MonadChecker m) => (PlumeType, PlumeType) -> m ()
unify (t1, t2) = do
  pos <- position <$> readIORef checkerST
  case pos of
    Just p -> do
      s <- readIORef checkerST
      writeIORef checkerST (s {constraints = s.constraints ++ [(p, t1 :~: t2)]})
    Nothing -> throw (CompilerError "No position found")

withVariables :: (MonadChecker m) => [(Text, Scheme)] -> m a -> m a
withVariables vars =
  with' @"variables" (fromList vars <>)

withReturnType :: (MonadChecker m) => PlumeType -> m a -> m a
withReturnType ret =
  with @"returnType" ret

withGenerics :: (MonadChecker m) => [(Text, Int)] -> m a -> m a
withGenerics gens =
  with' @"generics" (fromList gens <>)

with
  :: forall l a m b. (MonadChecker m, HasField l CheckerState a) => a -> m b -> m b
with v = with' @l (const v)

with'
  :: forall l a m b
   . (MonadChecker m, HasField l CheckerState a)
  => (a -> a)
  -> m b
  -> m b
with' f = local (\s -> setField @l s (f (getField @l s)))

createAnnotation :: Text -> (a -> b) -> a -> Annotation b
createAnnotation name f = (name :@:) . f

fromType :: (MonadChecker m) => Maybe Pre.PlumeType -> m PlumeType
fromType = maybe freshTVar convert

synthesize' :: Inference m Pre.Expression Expression
synthesize' (Pre.EVariable name) = do
  t <- search @"variables" name
  case t of
    Just t' -> do
      inst <- instantiate t'
      return (inst, Post.EVariable name inst)
    Nothing -> throw (UnboundVariable name)
synthesize' (Pre.EApplication f xs) = do
  (t, f') <- local id $ synthesize f
  (ts, xs') <- mapAndUnzipM synthesize xs
  ret <- freshTVar
  unify (t, ts :->: ret)
  return (ret, Post.EApplication f' xs')
synthesize' (Pre.ELiteral l) = return $ case l of
  LInt _ -> (Post.TInt, Post.ELiteral l)
  LBool _ -> (Post.TBool, Post.ELiteral l)
  LString _ -> (Post.TString, Post.ELiteral l)
  LChar _ -> (Post.TChar, Post.ELiteral l)
  LFloat _ -> (Post.TFloat, Post.ELiteral l)
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
  unify (t, ret')
  return
    ( args' :->: t
    , Post.EClosure
        (zipWith Annotation (map annotationName args) args')
        ret'
        body'
    )
synthesize' (Pre.EConditionBranch cond then' else') = case else' of
  Just else'' -> do
    (t, cond') <- synthesize cond
    (t', then'') <- synthesize then'
    (t'', else''') <- synthesize else''
    unify (t, Post.TBool)
    unify (t', t'')
    unify (t', t'')
    return (t', Post.EConditionBranch cond' then'' (Just else'''))
  Nothing -> do
    (t, cond') <- synthesize cond
    (t', then'') <- synthesize then'
    unify (t, Post.TBool)
    return (t', Post.EConditionBranch cond' then'' Nothing)
synthesize' (Pre.EBlock es) = do
  (ret, (_, es')) <- local id $ do
    es' <- mapAndUnzipM synthesize' es
    ret <- gets returnType
    return (ret, es')
  return (ret, Post.EBlock es')
synthesize' (Pre.ELocated e p) = do
  (t, e') <- local (\s -> s {position = Just p}) $ synthesize e
  return (t, Post.ELocated e' p)
synthesize' (Pre.EReturn e) = do
  (t, e') <- synthesize e
  ret <- gets returnType
  unify (t, ret)
  return (ret, Post.EReturn e')
synthesize' (Pre.EDeclaration gens (Annotation name ty) value body) = do
  (genericTys, genericNames) <- case gens of
    Just gens' -> (,gens') <$> mapM (const fresh) gens'
    Nothing -> return ([], [])
  let generics'' = zip genericNames genericTys
  ty' <- fromType ty
  (t, value') <-
    withVariables [(name, Forall genericTys ty')] $
      withGenerics generics'' $
        synthesize value
  unify (t, ty')
  case body of
    Just body' -> do
      (ret, body'') <-
        withVariables [(name, Forall genericTys ty')] $
          synthesize body'
      return
        ( ret
        , Post.EDeclaration
            (Annotation name ty')
            value'
            (Just body'')
        )
    Nothing ->
      return (ty', Post.EDeclaration (Annotation name ty') value' Nothing)
synthesize' (Pre.ESwitch e cases) = do
  (t, e') <- synthesize e
  (cases', ts) <-
    mapAndUnzipM
      ( \(p, e'') -> do
          (patternTy, p', env) <- synthesizePat p
          (exprTy, e''') <- withVariables (M.toList env) $ synthesize e''
          unify (t, patternTy)
          return ((p', e'''), exprTy)
      )
      cases
  (ret, xs) <- case ts of
    [] -> throw EmptyMatch
    (ret : xs) -> return (ret, xs)

  forM_ xs $ unify . (ret,)

  return (ret, Post.ESwitch e' cases')
synthesize' (Pre.ETypeExtension {}) = throw (CompilerError "Type extensions are not supported yet")

synthesizePat
  :: (MonadChecker m)
  => Pre.Pattern
  -> m (Post.PlumeType, Pattern, Map Text Scheme)
synthesizePat (Pre.PVariable name) = do
  t <- search @"types" name
  case t of
    Just t' -> do
      inst <- instantiate t'
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
      inst <- instantiate t
      (ts, pats', vars) <- mapAndUnzip3M synthesizePat pats
      unify (inst, ts :->: tv)
      return (tv, Post.PConstructor name pats', mconcat vars)
    Nothing -> throw (UnboundVariable name)

synthesize :: Inference m Pre.Expression Expression
synthesize = local id . synthesize'

mapAndUnzip3M :: (Monad m) => (a -> m (b, c, d)) -> [a] -> m ([b], [c], [d])
mapAndUnzip3M f =
  foldrM
    ( \x (bs, cs, ds) -> do
        (b, c, d) <- f x
        return (b : bs, c : cs, d : ds)
    )
    ([], [], [])