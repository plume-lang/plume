{-# LANGUAGE LambdaCase #-}

module Plume.TypeChecker.Checker where

import Data.List qualified as List
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation qualified as Cmm
import Plume.Syntax.Translation.Generics (concatMapM)
import Plume.TypeChecker.Checker.Application
import Plume.TypeChecker.Checker.Closure
import Plume.TypeChecker.Checker.Condition
import Plume.TypeChecker.Checker.Datatype
import Plume.TypeChecker.Checker.Declaration
import Plume.TypeChecker.Checker.While
import Plume.TypeChecker.Checker.Extension
import Plume.TypeChecker.Checker.Interface (synthInterface)
import Plume.TypeChecker.Checker.Native
import Plume.TypeChecker.Checker.Switch
import Plume.TypeChecker.Constraints.Solver
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Constraints.Typeclass
import Plume.TypeChecker.Monad
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post
import Prelude hiding (gets, local, modify)
import Data.List (unzip4)

synthesize :: (MonadChecker m) => Pre.Expression -> m (PlumeType, [PlumeQualifier], Placeholder Post.Expression, Bool)

-- | Some basic and primitive expressions
synthesize (Pre.ELocated expr pos) = withPosition pos $ synthesize expr

{-  
 -  x : (β ⇒ σ) ∈ Γ
 -  ---------------
 -  Γ ⊦ x : (β ⇒ σ)
 -}
synthesize (Pre.EVariable name _) = do
  -- Checking if the variable is a value
  searchEnv @"typeEnv" name.identifier >>= \case
    Just scheme -> instantiateFromName name.identifier scheme
    Nothing ->
      -- Checking if the variable is a data-type constructor
      searchEnv @"datatypeEnv" name.identifier >>= \case
        Just sch -> do
          (ty, qs) <- instantiate sch
          pure (ty, qs, pure (Post.EVariable name (Identity ty)), False)
        Nothing -> throw (UnboundVariable name.identifier)
synthesize (Pre.ELiteral lit) = do
  let (ty, lit') = typeOfLiteral lit
  pure (ty, [], pure (Post.ELiteral lit'), False)
synthesize (Pre.EUnMut e) = do
  tv <- fresh
  (ty, ps, r, isAsync) <- synthesize e
  ty `unifiesWith` TMut tv
  pure (tv, ps, Post.EUnMut <$> r, isAsync)
synthesize (Pre.EBlock exprs) = local id $ do
  (tys, pss, exprs', isAsync) <- localPosition $ unzip4 <$> mapM synthesizeStmt exprs

  retTy <- gets returnType
  let retTy' = fromMaybe TUnit retTy

  let tys' = catMaybes tys

  gotRetTy <- case tys' of
    [] -> fresh
    (t:ys) -> forM_ ys (unifiesWith t) $> t

  -- void $ maybeM (viaNonEmpty last tys) (`unifiesWith` retTy')

  return (gotRetTy, concat pss, liftBlock (Post.EBlock <$> sequence exprs') tys' retTy', or isAsync)
synthesize (Pre.EReturn expr) = do
  (ty, ps, expr', isAsync) <- synthesize expr
  returnTy <- gets returnType
  forM_ returnTy $ unifiesWith ty
  pure (ty, ps, Post.EReturn <$> expr', isAsync)
synthesize (Pre.EList xs) = do
  tv <- fresh
  (tys, pss, xs', isAsync) <-
    mapAndUnzip4M
      (local id . localPosition . synthesize)
      xs
  forM_ tys $ unifiesWith tv
  pure (TList tv, concat pss, Post.EList <$> sequence xs', or isAsync)
synthesize (Pre.EVariableDeclare gens name ty) = do
  gens' <- concatMapM convert gens
  let qvars = getQVars gens'
  let quals = removeQVars gens'
  ty' <- convert ty

  insertEnv @"typeEnv" name (Forall qvars (quals :=>: ty'))

  pure (ty', [], pure (Post.EVariableDeclare gens name (Identity ty')), False)
-- \| Calling synthesis modules
synthesize app@(Pre.EApplication {}) = synthApp synthesize app
synthesize clos@(Pre.EClosure {}) = synthClosure synthesize clos
synthesize decl@(Pre.EDeclaration {}) = synthDecl False synthesize decl
synthesize whil@(Pre.EWhile {}) = synthWhile synthesize whil
synthesize cond@(Pre.EConditionBranch {}) = synthCond synthesize cond
synthesize ext@(Pre.ETypeExtension {}) = synthExt synthesize ext
synthesize ty@(Pre.EType {}) = synthDataType ty
synthesize sw@(Pre.ESwitch {}) = synthSwitch synthesize sw
synthesize int@(Pre.EInterface {}) = synthInterface synthesize int
synthesize nat@(Pre.ENativeFunction {}) = synthNative nat
synthesize _ = throw (CompilerError "Unsupported expression")

synthesizeStmt :: (MonadChecker m) => Pre.Expression -> m (Maybe PlumeType, [PlumeQualifier], Placeholder Post.Expression, Bool)
synthesizeStmt (Pre.ELocated e pos) = withPosition pos $ synthesizeStmt e
synthesizeStmt (Pre.EReturn e) = do
  (ty, ps, expr', isAsync) <- synthesize e
  returnTy <- gets returnType
  forM_ returnTy $ unifiesWith ty
  pure (Just ty, ps, Post.EReturn <$> expr', isAsync)
synthesizeStmt e = do
  (_, ps, h, isAsync) <- synthesize e
  return (Nothing, ps, h, isAsync)

isClosure :: Pre.Expression -> Bool
isClosure (Pre.ELocated e _) = isClosure e
isClosure (Pre.EClosure {}) = True
isClosure _ = False

synthesizeToplevel :: (MonadChecker m) => Pre.Expression -> m (PlumeScheme, [Post.Expression])
synthesizeToplevel (Pre.ELocated e pos) = withPosition pos $ synthesizeToplevel e
synthesizeToplevel e@(Pre.EDeclaration _ _ body _) = do
  (ty, ps, h, _) <- synthDecl (isClosure body) synthesize e
  cenv <- gets (extendEnv . environment)
  zs <- traverse (discharge cenv) ps

  let (ps', m, as, _) = mconcat zs
  (_, as') <- removeDuplicatesAssumps as
  -- ps'' <- removeDuplicatesQuals ps'
  let t'' = Forall [] $ List.nub ps' :=>: ty

  pos <- fetchPosition
  checkSub <- gets substitution
  h' <- liftIO $ runReaderT h $ getExpr pos checkSub m

  unless (null as') $ do
    throw (UnresolvedTypeVariable as')

  case h' of
    Post.ESpreadable es -> pure (t'', es)
    _ -> pure (t'', [h'])
synthesizeToplevel e = do
  (ty, ps, h, _) <- synthesize e
  cenv <- gets (extendEnv . environment)
  zs <- traverse (discharge cenv) (reverse ps)

  let (ps', m, as, _) = mconcat zs
  (_, as') <- removeDuplicatesAssumps as
  -- ps'' <- removeDuplicatesQuals ps'
  let t'' = Forall [] $ List.nub ps' :=>: ty

  pos <- fetchPosition
  checkSub <- gets substitution
  h' <- liftIO $ runReaderT h $ getExpr pos checkSub m

  unless (null as') $ do
    throw (UnresolvedTypeVariable as')

  case h' of
    Post.ESpreadable es -> pure (t'', es)
    _ -> pure (t'', [h'])

-- | Locally synthesize a list of expressions
synthesizeMany :: (MonadChecker m) => [Pre.Expression] -> m [Post.Expression]
synthesizeMany = concatMapM (fmap snd . localPosition . synthesizeToplevel)

runSynthesize :: (MonadIO m) => [Pre.Expression] -> m (Either PlumeError [Post.Expression])
runSynthesize = runExceptT . synthesizeMany
