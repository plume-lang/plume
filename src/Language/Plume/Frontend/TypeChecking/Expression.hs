module Language.Plume.Frontend.TypeChecking.Expression where

import Language.Plume.Syntax.HLIR qualified as HLIR
import Language.Plume.Frontend.TypeChecking.Unification qualified as U
import Language.Plume.Frontend.TypeChecking.Monad qualified as M
import Data.Map qualified as Map
import Control.Monad.Result qualified as Err
import Control.Monad.Position qualified as Pos

typecheckE
  :: M.MonadChecker m
  => HLIR.AST "expression"
  -> m (HLIR.HLIR "expression", HLIR.PlumeType)
typecheckE (HLIR.MkExprVariable ann) = do
  st <- M.get
  case Map.lookup ann.name st.variables of
    Just sch -> do
      ty <- M.instantiate sch
      pure (HLIR.MkExprVariable (ann { HLIR.value = Identity ty }), ty)
    Nothing -> M.throw (Err.VariableNotFound ann.name)
typecheckE (HLIR.MkExprCall callee args _) = do
  (newCallee, calleeTy) <- typecheckE callee
  (newArgs, newArgsTys) <- mapAndUnzipM typecheckE args

  newRetTy <- M.newTypeVar
  let newFunTy = newArgsTys HLIR.:->: newRetTy

  calleeTy `U.unify` newFunTy

  pure (HLIR.MkExprCall newCallee newArgs (Identity newFunTy), newRetTy)
typecheckE (HLIR.MkExprIf cond th el) = do
  (newCond, condTy) <- typecheckE cond
  (newTh, thTy) <- typecheckE th
  (newEl, elTy) <- typecheckE el

  condTy `U.unify` HLIR.MkTyBool
  thTy `U.unify` elTy

  pure (HLIR.MkExprIf newCond newTh newEl, thTy)
typecheckE (HLIR.MkExprLambda args ret body) = do
  let argsNames = map (.name) args
  preArgsTys  <- mapM (const M.newTypeVar) args
  preRetTy <- M.newTypeVar

  let preFunTy = preArgsTys HLIR.:->: preRetTy

  let newEnv = Map.fromList $
        zipWith (\argName ty ->
          (argName, HLIR.MkTyScheme [] ty))
        argsNames preArgsTys

  M.modify $ \st -> st { M.variables = newEnv <> st.variables }

  (newBody, newRetTy) <- M.local (\s -> s { M.returnType = ret }) $ typecheckE body

  preRetTy `U.unify` newRetTy

  let wfRetTy = Identity preRetTy
  let wfArgs = map
          (\(argName, HLIR.MkTyScheme _ ty) ->
              HLIR.MkAnnotation argName (Identity ty))
          (Map.toList newEnv)

  pure (HLIR.MkExprLambda wfArgs wfRetTy newBody, preFunTy)
typecheckE (HLIR.MkExprLocated pos e) =
  Pos.pushPosition pos *> typecheckE e <* Pos.popPosition
typecheckE (HLIR.MkExprLiteral lit) = do
  let typeOfLit = case lit of
        HLIR.MkInteger _ -> HLIR.MkTyInt
        HLIR.MkFloat _ -> HLIR.MkTyFloat
        HLIR.MkString _ -> HLIR.MkTyString
        HLIR.MkChar _ -> HLIR.MkTyChar

  pure (HLIR.MkExprLiteral lit, typeOfLit)
typecheckE (HLIR.MkExprBlock es _) = do
  (newEs, _) <- mapAndUnzipM typecheckS es

  retTy <- fromMaybe HLIR.MkTyUnit <$> M.gets M.returnType

  pure (HLIR.MkExprBlock newEs (Identity retTy), retTy)
typecheckE (HLIR.MkExprAnnotation e ann) = do
  (newE, ty) <- typecheckE e
  ty `U.unify` ann
  pure (HLIR.MkExprAnnotation newE ann, ann)
typecheckE (HLIR.MkExprReturn e) = typecheckE e

typecheckS
  :: M.MonadChecker m
  => HLIR.AST "expression"
  -> m (HLIR.HLIR "expression", HLIR.PlumeType)
typecheckS (HLIR.MkExprLocated pos e) =
  Pos.pushPosition pos *> typecheckS e <* Pos.popPosition
typecheckS (HLIR.MkExprReturn e) = do
  (newE, ty) <- typecheckE e
  retTy <- M.gets M.returnType

  let retTy' = fromMaybe HLIR.MkTyUnit retTy
  
  ty `U.unify` retTy'
  
  pure (HLIR.MkExprReturn newE, ty)
typecheckS e = do
  (newE, _) <- typecheckE e
  pure (newE, HLIR.MkTyUnit)
