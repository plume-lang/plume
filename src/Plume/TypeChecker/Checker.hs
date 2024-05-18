{-# LANGUAGE LambdaCase #-}

module Plume.TypeChecker.Checker where

import Data.List qualified as List
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Translation.Generics (concatMapM)
import Plume.TypeChecker.Checker.Application
import Plume.TypeChecker.Checker.Closure
import Plume.TypeChecker.Checker.Condition
import Plume.TypeChecker.Checker.Datatype
import Plume.TypeChecker.Checker.Declaration
import Plume.TypeChecker.Checker.Extension
import Plume.TypeChecker.Checker.Interface (synthInterface)
import Plume.TypeChecker.Checker.Native
import Plume.TypeChecker.Checker.Switch
import Plume.TypeChecker.Constraints.Solver
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Constraints.Typeclass
import Plume.TypeChecker.Monad
import Plume.TypeChecker.TLIR qualified as Post
import Prelude hiding (gets, local)

synthesize :: (MonadChecker m) => Pre.Expression -> m (PlumeType, [PlumeQualifier], Placeholder Post.Expression)

-- | Some basic and primitive expressions
synthesize (Pre.ELocated expr pos) = withPosition pos $ synthesize expr
synthesize (Pre.EVariable name) = do
  -- Checking if the variable is a value
  searchEnv @"typeEnv" name >>= \case
    Just scheme -> instantiateFromName name scheme
    Nothing ->
      -- Checking if the variable is a data-type constructor or variable
      searchEnv @"datatypeEnv" name >>= \case
        Just sch -> do
          (ty, qs) <- instantiate sch
          pure (ty, qs, pure (Post.EVariable name ty))
        Nothing -> throw (UnboundVariable name)
synthesize (Pre.ELiteral lit) = do
  let (ty, lit') = typeOfLiteral lit
  pure (ty, [], pure (Post.ELiteral lit'))
synthesize (Pre.EUnMut e) = do
  tv <- fresh
  (ty, ps, r) <- synthesize e
  ty `unifiesWith` TMut tv
  pure (tv, ps, Post.EUnMut <$> r)
synthesize (Pre.EBlock exprs) = local id $ do
  (tys, pss, exprs') <-
    mapAndUnzip3M
      (localPosition . synthesize)
      exprs

  retTy <- gets returnType
  let retTy' = fromMaybe TUnit retTy

  return (retTy', concat pss, liftBlock (Post.EBlock <$> sequence exprs') tys retTy')
synthesize (Pre.EReturn expr) = do
  (ty, ps, expr') <- synthesize expr
  returnTy <- gets returnType
  forM_ returnTy $ unifiesWith ty
  pure (ty, ps, Post.EReturn <$> expr')
synthesize (Pre.EList xs) = do
  tv <- fresh
  (tys, pss, xs') <-
    mapAndUnzip3M
      (local id . localPosition . synthesize)
      xs
  forM_ tys $ unifiesWith tv
  pure (TList tv, concat pss, Post.EList <$> sequence xs')
-- \| Calling synthesis modules
synthesize app@(Pre.EApplication {}) = synthApp synthesize app
synthesize clos@(Pre.EClosure {}) = synthClosure synthesize clos
synthesize decl@(Pre.EDeclaration {}) = synthDecl synthesize decl
synthesize cond@(Pre.EConditionBranch {}) = synthCond synthesize cond
synthesize ext@(Pre.ETypeExtension {}) = synthExt synthesize ext
synthesize ty@(Pre.EType {}) = synthDataType ty
synthesize sw@(Pre.ESwitch {}) = synthSwitch synthesize sw
synthesize int@(Pre.EInterface {}) = synthInterface synthesize int
synthesize nat@(Pre.ENativeFunction {}) = synthNative nat

synthesizeToplevel :: (MonadChecker m) => Pre.Expression -> m (PlumeScheme, [Post.Expression])
synthesizeToplevel (Pre.ELocated e pos) = withPosition pos $ synthesizeToplevel e
synthesizeToplevel e = do
  (pos, (ty, ps, h)) <- getPosition $ synthesize e
  cenv <- gets (extendEnv . environment)
  zs <- traverse (discharge cenv) ps

  let (ps', m, as, _) = mconcat zs
  (_, as') <- removeDuplicatesAssumps as
  ps'' <- removeDuplicatesQuals ps'
  let t'' = Forall [] $ List.nub ps'' :=>: ty
  h' <- liftIO $ runReaderT h $ getExpr m

  unless (null as') $ do
    throwRaw (pos, UnresolvedTypeVariable as')

  case h' of
    Post.ESpreadable es -> pure (t'', es)
    _ -> pure (t'', [h'])

-- | Locally synthesize a list of expressions
synthesizeMany :: (MonadChecker m) => [Pre.Expression] -> m [Post.Expression]
synthesizeMany = concatMapM (fmap snd . localPosition . synthesizeToplevel)

runSynthesize :: (MonadIO m) => [Pre.Expression] -> m (Either PlumeError [Post.Expression])
runSynthesize = runExceptT . synthesizeMany
