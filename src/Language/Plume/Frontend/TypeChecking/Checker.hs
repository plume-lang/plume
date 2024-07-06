module Language.Plume.Frontend.TypeChecking.Checker where

import Language.Plume.Syntax.HLIR qualified as HLIR
import Language.Plume.Frontend.TypeChecking.Monad qualified as M
import Data.Map qualified as Map
import Language.Plume.Frontend.TypeChecking.Unification qualified as U
import Control.Monad.Position qualified as Pos
import Language.Plume.Frontend.TypeChecking.Expression qualified as Expr
import Language.Plume.Frontend.Module.Resolution qualified as Res
import Data.List qualified as List

typecheckD
  :: M.MonadChecker m
  => HLIR.AST "declaration"
  -> m (HLIR.HLIR "declaration", HLIR.PlumeType)
typecheckD (HLIR.MkDeclVariable ann _ expr) = do
  let name = ann.name

  newDeclTy <- M.newTypeVar
  let newDeclSch = HLIR.MkTyScheme [] newDeclTy
  M.modify $ \st -> st {
    M.variables = Map.insert name newDeclSch st.variables
  }

  (newExpr, ty) <- Expr.typecheckE expr

  newDeclTy `U.unify` ty
  finalScheme <- M.generalize newDeclTy

  M.modify $ \st -> st {
    M.variables = Map.insert name finalScheme st.variables
  }

  qvars <- List.nub <$> M.getFreeVars ty
  qvars' <- mapM (\qv -> do
      tv <- newIORef $ HLIR.Unbound qv 0
      pure (HLIR.MkTyVar tv)
    ) qvars

  newDeclTy `U.unifyWithMaybe` ann.value

  pure (HLIR.MkDeclVariable (ann { HLIR.value = Identity newDeclTy }) qvars'  newExpr, newDeclTy)
typecheckD (HLIR.MkDeclFunction name generics args ret body) = do
  let argsNames = map (.name) args

  preArgsTys <- mapM (const M.newTypeVar) argsNames
  preRetTy <- M.newTypeVar

  let preFunTy = preArgsTys HLIR.:->: preRetTy
  let funSch = HLIR.MkTyScheme [] preFunTy

  let newEnv = Map.fromList $
        zipWith (\argName ty ->
          (argName, HLIR.MkTyScheme [] ty))
        argsNames preArgsTys

  (newBody, newRetTy) <-
    M.local (\st -> st {
      M.variables = Map.insert name funSch st.variables <> newEnv,
      M.returnType = Just preRetTy
    }) $
      Expr.typecheckE body

  preRetTy `U.unify` newRetTy
  finalScheme <- M.generalize preFunTy

  M.modify $ \st -> st {
    M.variables = Map.insert name finalScheme st.variables
  }

  -- Type checking user-given annotations
  genericsMap <- Map.fromList <$> mapM (M.generateGeneric . Res.getGeneric) generics
  M.local (\st -> st { M.generics = genericsMap }) $ do
    preRetTy `U.unifyWithMaybe` ret
    zipWithM_ (\(_, HLIR.MkTyScheme _ t1) (HLIR.MkAnnotation _ t2) ->
        t1 `U.unifyWithMaybe` t2
      ) (Map.toList newEnv) args

  let wfArgs = map
        (\(argName, HLIR.MkTyScheme _ ty) ->
            HLIR.MkAnnotation argName (Identity ty))
        (Map.toList newEnv)
  let wfRet = Identity preRetTy

  pure (HLIR.MkDeclFunction name (Map.elems genericsMap) wfArgs wfRet newBody, preFunTy)
typecheckD (HLIR.MkDeclLocated pos e) =
  Pos.pushPosition pos *> typecheckD e <* Pos.popPosition
typecheckD (HLIR.MkDeclPublic e) = typecheckD e
typecheckD (HLIR.MkDeclRequire p) = do
  pure (HLIR.MkDeclRequire p, HLIR.MkTyUnit)
typecheckD (HLIR.MkDeclNative name generics args ret) = do
  let funTy = args HLIR.:->: ret
  let funSch = HLIR.MkTyScheme (map Res.getGeneric generics) funTy

  generics' <-
    Map.fromList <$>
      mapM
        (M.generateGeneric . Res.getGeneric)
        generics
  newArgsTys <- mapM (M.substType generics') args
  newRetTy <- M.substType generics' ret

  M.modify $ \st -> st {
    M.variables = Map.insert name funSch st.variables
  }

  pure (
    HLIR.MkDeclNative name (Map.elems generics') newArgsTys newRetTy,
    newArgsTys HLIR.:->: newRetTy)
typecheckD (HLIR.MkDeclGenericProperty name generics ty) = do
  let funSch = HLIR.MkTyScheme (map Res.getGeneric generics) ty

  generics' <-
    Map.fromList <$>
      mapM
        (M.generateGeneric . Res.getGeneric)
        generics

  ty' <- M.substType generics' ty

  modifyIORef' M.templates (Map.insert name funSch)

  pure (
    HLIR.MkDeclGenericProperty name (Map.elems generics') ty', ty')
typecheckD (HLIR.MkDeclExtend generics name args ret body) = do
  let argsNames = map (.name) args

  preArgsTys <- mapM (const M.newTypeVar) argsNames
  preRetTy <- M.newTypeVar

  let preFunTy = preArgsTys HLIR.:->: preRetTy
  let funSch = HLIR.MkTyScheme [] preFunTy

  let newEnv = Map.fromList $
        zipWith (\argName ty ->
          (argName, HLIR.MkTyScheme [] ty))
        argsNames preArgsTys

  (newBody, newRetTy) <-
    M.local (\st -> st {
      M.variables = Map.insert name funSch st.variables <> newEnv,
      M.returnType = Just preRetTy
    }) $
      Expr.typecheckE body

  preRetTy `U.unify` newRetTy

  -- Type checking user-given annotations
  genericsMap <- Map.fromList <$> mapM (M.generateGeneric . Res.getGeneric) generics
  M.local (\st -> st { M.generics = genericsMap }) $ do
    preRetTy `U.unifyWithMaybe` ret
    zipWithM_ (\(_, HLIR.MkTyScheme _ t1) (HLIR.MkAnnotation _ t2) ->
        t1 `U.unifyWithMaybe` t2
      ) (Map.toList newEnv) args

  let wfArgs = map
        (\(argName, HLIR.MkTyScheme _ ty) ->
            HLIR.MkAnnotation argName (Identity ty))
        (Map.toList newEnv)
  let wfRet = Identity preRetTy

  pure (HLIR.MkDeclExtend (Map.elems genericsMap) name wfArgs wfRet newBody, preFunTy)