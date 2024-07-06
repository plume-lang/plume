{-# LANGUAGE LambdaCase #-}
module Language.Plume.Frontend.TypeChecking.Expression where

import Language.Plume.Syntax.HLIR qualified as HLIR
import Language.Plume.Frontend.TypeChecking.Unification qualified as U
import Language.Plume.Frontend.TypeChecking.Monad qualified as M
import Data.Map qualified as Map
import Control.Monad.Result qualified as Err
import Control.Monad.Position qualified as Pos
import Language.Plume.Backend.Closure.Conversion (mapAndUnzip3M)

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
    Nothing -> do
      templates <- readIORef M.templates
      case Map.lookup ann.name templates of
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
typecheckE (HLIR.MkExprLocated pos e) = do
  Pos.pushPosition pos
  (e', ty) <- typecheckE e
  void Pos.popPosition

  pure (HLIR.MkExprLocated pos e', ty)
typecheckE (HLIR.MkExprLiteral lit) = do
  let typ = typeOfLit lit

  pure (HLIR.MkExprLiteral lit, typ)
typecheckE (HLIR.MkExprBlock es _) = do
  (newEs, _) <- mapAndUnzipM typecheckS es

  retTy <- fromMaybe HLIR.MkTyUnit <$> M.gets M.returnType

  pure (HLIR.MkExprBlock newEs (Identity retTy), retTy)
typecheckE (HLIR.MkExprAnnotation e ann) = do
  (newE, ty) <- typecheckE e
  ty `U.unify` ann
  pure (HLIR.MkExprAnnotation newE ann, ann)
typecheckE (HLIR.MkExprReturn e) = typecheckE e
typecheckE (HLIR.MkExprSwitch e cases _) = do
  (newE, ty) <- typecheckE e
  (caseTys, newCases) <- mapAndUnzipM (synthCase ty) cases
  
  forM_ caseTys $ \(patTy, _) -> ty `U.unify` patTy
  
  ((_, x), xs) <- case caseTys of
    [] -> M.throw Err.EmptySwitch
    (x:xs) -> pure (x, xs)

  forM_ xs $ \(_, exprTy) -> x `U.unify` exprTy

  pure (HLIR.MkExprSwitch newE newCases (Identity ty), ty)

typeOfLit :: HLIR.Literal -> HLIR.PlumeType
typeOfLit = \case
  HLIR.MkInteger _ -> HLIR.MkTyInt
  HLIR.MkFloat _ -> HLIR.MkTyFloat
  HLIR.MkString _ -> HLIR.MkTyString
  HLIR.MkChar _ -> HLIR.MkTyChar
  HLIR.MkBool _ -> HLIR.MkTyBool

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

synthCase
  :: M.MonadChecker m
  => HLIR.PlumeType
  -> (HLIR.AST "pattern", HLIR.AST "expression")
  -> m (
      (HLIR.PlumeType, HLIR.PlumeType), 
      (HLIR.HLIR "pattern", HLIR.HLIR "expression")
    )
synthCase scrutTy (pat, expr) = M.local id $ do
  -- Synthesize the pattern and infer the expression
  (patTy, patExpr, patEnv) <- synthPattern pat

  -- Pattern type should unify with the scrutinee type
  scrutTy `U.unify` patTy

  (expr', exprTy) <- M.local (\s -> s { M.variables = patEnv <> s.variables }) (typecheckE expr)
  pure ((patTy, exprTy), (patExpr, expr'))

synthPattern
  :: M.MonadChecker m
  => HLIR.AST "pattern"
  -> m (HLIR.PlumeType, HLIR.HLIR "pattern", Map Text HLIR.PlumeScheme)
synthPattern HLIR.MkPatternWildcard{} = do
  t <- M.newTypeVar
  pure (t, HLIR.MkPatternWildcard (Identity t), mempty)
synthPattern (HLIR.MkPatternVariable ann) = do
  env <- M.gets M.variables
  let t = Map.lookup ann.name env
  case t of
    Just t' -> do
      inst <- M.instantiate t'
      let ann' = HLIR.MkAnnotation ann.name (Identity inst)
      return (inst, HLIR.MkPatternSpecial ann' [], mempty)
    Nothing -> do
      ty <- M.newTypeVar
      let ann' = HLIR.MkAnnotation ann.name (Identity ty)
      return
        ( ty
        , HLIR.MkPatternVariable ann'
        , Map.singleton ann.name (HLIR.MkTyScheme [] ty)
        )
synthPattern (HLIR.MkPatternLiteral l) = do
  let ty = typeOfLit l
  pure (ty, HLIR.MkPatternLiteral l, mempty)
synthPattern (HLIR.MkPatternLocated pos p) =
  Pos.pushPosition pos *> synthPattern p <* Pos.popPosition
synthPattern (HLIR.MkPatternSpecial ann pats) = do
  env <- M.gets M.variables
  let t = Map.lookup ann.name env
  case t of
    Just t' -> do
      inst <- M.instantiate t'
      ret <- M.newTypeVar
      (patsTy, pats', env') <- mapAndUnzip3M synthPattern pats
      inst `U.unify` (patsTy HLIR.:->: ret)

      let ann' = HLIR.MkAnnotation ann.name (Identity inst)

      return (ret, HLIR.MkPatternSpecial ann' pats', mconcat env')
    Nothing -> M.throw $ Err.VariableNotFound ann.name