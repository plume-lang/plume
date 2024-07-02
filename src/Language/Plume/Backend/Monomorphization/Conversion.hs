module Language.Plume.Backend.Monomorphization.Conversion where

import Language.Plume.Backend.Monomorphization.Monad qualified as M
import Language.Plume.Syntax.MLIR qualified as MLIR
import Language.Plume.Frontend.TypeChecking.Monad qualified as M
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

createSchemeFrom
  :: M.MonadMono m
  => [MLIR.PlumeType]
  -> MLIR.PlumeType
  -> m MLIR.PlumeScheme
createSchemeFrom gens ty = do
  qvars <- concat <$> mapM M.getFreeVars gens

  pure $ MLIR.MkTyScheme qvars ty

convertMono
  :: M.MonadMono m
  => MLIR.MLIR "declaration"
  -> m ()
convertMono (MLIR.MkDeclFunction name gens args ret body) = do
  let argsTys = map (.value) args
  let funTy = argsTys MLIR.:->: ret
  scheme <- createSchemeFrom gens funTy

  let argsSet = Set.fromList $ map (.name) args

  body' <- M.withLocals argsSet $ convertMonoE body

  let fun = MLIR.MkDeclFunction name gens args ret body'

  b <- M.containsTypeVar funTy
  when b $ do
    modifyIORef' M.functionTable (Map.insert name (scheme, fun))

  unless b $ do
    modifyIORef' M.resultState (<> [fun])
convertMono (MLIR.MkDeclVariable ann qvs expr) = do
  expr' <- convertMonoE expr

  let var = MLIR.MkDeclVariable ann qvs expr'

  b <- M.containsTypeVar ann.value
  when b $ do
    scheme <- createSchemeFrom qvs ann.value
    modifyIORef' M.functionTable (Map.insert ann.name (scheme, var))

  unless b $ do
    modifyIORef' M.resultState (<> [var])
convertMono (MLIR.MkDeclNative name gens args ret) = do
  modifyIORef' M.reserved (Set.insert name)

  modifyIORef M.resultState (<> [MLIR.MkDeclNative name gens args ret])

convertMonoE
  :: M.MonadMono m
  => MLIR.MLIR "expression"
  -> m (MLIR.MLIR "expression")
convertMonoE (MLIR.MkExprLiteral lit) =
  pure $ MLIR.MkExprLiteral lit
convertMonoE (MLIR.MkExprVariable name ty) = do
  reserved <- readIORef M.reserved
  locals <- readIORef M.locals

  if Set.member name reserved || Set.member name locals
    then pure $ MLIR.MkExprVariable name ty
    else do
      scheme <- readIORef M.functionTable

      case Map.lookup name scheme of
        Just (MLIR.MkTyScheme _ sch, body) -> do
          subst <- M.substFromUnify sch ty
          newTy <- M.apply subst sch

          b <- M.containsTypeVar newTy
          if b
            then pure $ MLIR.MkExprVariable name ty
            else do
              let tys = Map.elems subst
              let formatName = name <> "_" <> Text.intercalate "_" (map show tys)

              let newBody = replaceNameWith name formatName body
              substBody <- applyD subst newBody

              convertMono substBody

              pure $ MLIR.MkExprVariable formatName newTy
        Nothing -> pure $ MLIR.MkExprVariable name ty
convertMonoE (MLIR.MkExprLambda args ret body) = do
  let argsSet = Set.fromList $ map (.name) args
  body' <- M.withLocals argsSet $ convertMonoE body
  pure $ MLIR.MkExprLambda args ret body'
convertMonoE (MLIR.MkExprCall callee args t) = do
  callee' <- convertMonoE callee
  args' <- mapM convertMonoE args

  pure $ MLIR.MkExprCall callee' args' t
convertMonoE (MLIR.MkExprLet name ty expr bTy body) = do
  expr' <- convertMonoE expr
  body' <- traverse convertMonoE body

  pure $ MLIR.MkExprLet name ty expr' bTy body'
convertMonoE (MLIR.MkExprIf cond then_ else_) = do
  cond' <- convertMonoE cond
  then_' <- convertMonoE then_
  else_' <- convertMonoE else_

  pure $ MLIR.MkExprIf cond' then_' else_'
convertMonoE (MLIR.MkExprBlock exprs t) = do
  exprs' <- mapM convertMonoE exprs

  pure $ MLIR.MkExprBlock exprs' t
convertMonoE (MLIR.MkExprField e f t) = do
  e' <- convertMonoE e

  pure $ MLIR.MkExprField e' f t
convertMonoE (MLIR.MkExprTupleAccess e i t) = do
  e' <- convertMonoE e

  pure $ MLIR.MkExprTupleAccess e' i t
convertMonoE (MLIR.MkExprPack anns (fun, env) ty) = do
  fun' <- convertMonoE fun
  env' <- mapM (traverse convertMonoE) env

  case ty of
    MLIR.MkTyExists qv t -> do
      let s = Map.singleton qv MLIR.MkTyAny
      t' <- M.apply s t
      env'' <- mapM (traverse (applyE s)) env'
      fun'' <- applyE s fun

      pure $ MLIR.MkExprPack anns (fun'', env'') t'
    _ -> do
      pure $ MLIR.MkExprPack anns (fun', env') ty
convertMonoE (MLIR.MkExprClosureCall e args t) = do
  e' <- convertMonoE e
  args' <- mapM convertMonoE args

  pure $ MLIR.MkExprClosureCall e' args' t
convertMonoE (MLIR.MkExprReturn e) = do
  e' <- convertMonoE e

  pure $ MLIR.MkExprReturn e'

applyE
  :: MonadIO m
  => Map MLIR.QuVar MLIR.PlumeType
  -> MLIR.MLIR "expression"
  -> m (MLIR.MLIR "expression")
applyE subst (MLIR.MkExprVariable name ty) = do
  ty' <- M.apply subst ty
  pure $ MLIR.MkExprVariable name ty'
applyE subst (MLIR.MkExprLambda args ret body) = do
  args' <- mapM (traverse (M.apply subst)) args
  ret' <- M.apply subst ret
  body' <- applyE subst body

  pure $ MLIR.MkExprLambda args' ret' body'
applyE subst (MLIR.MkExprCall callee args t) = do
  callee' <- applyE subst callee
  args' <- mapM (applyE subst) args
  t' <- M.apply subst t

  pure $ MLIR.MkExprCall callee' args' t'
applyE subst (MLIR.MkExprLet name ty expr bTy body) = do
  ty' <- M.apply subst ty
  expr' <- applyE subst expr
  body' <- traverse (applyE subst) body
  bTy' <- M.apply subst bTy

  pure $ MLIR.MkExprLet name ty' expr' bTy' body'
applyE subst (MLIR.MkExprIf cond then_ else_) = do
  cond' <- applyE subst cond
  then_' <- applyE subst then_
  else_' <- applyE subst else_

  pure $ MLIR.MkExprIf cond' then_' else_'
applyE subst (MLIR.MkExprBlock exprs t) = do
  exprs' <- mapM (applyE subst) exprs
  t' <- M.apply subst t

  pure $ MLIR.MkExprBlock exprs' t'
applyE subst (MLIR.MkExprField e f t) = do
  e' <- applyE subst e
  t' <- M.apply subst t

  pure $ MLIR.MkExprField e' f t'
applyE subst (MLIR.MkExprTupleAccess e i t) = do
  e' <- applyE subst e
  t' <- M.apply subst t

  pure $ MLIR.MkExprTupleAccess e' i t'
applyE subst (MLIR.MkExprPack anns (fun, env) ty) = do
  fun' <- applyE subst fun
  env' <- mapM (traverse (applyE subst)) env
  ty' <- M.apply subst ty

  pure $ MLIR.MkExprPack anns (fun', env') ty'
applyE _ (MLIR.MkExprLiteral lit) =
  pure $ MLIR.MkExprLiteral lit
applyE s (MLIR.MkExprClosureCall e args t) =
  MLIR.MkExprClosureCall <$> applyE s e <*> mapM (applyE s) args <*> pure t
applyE s (MLIR.MkExprReturn e) = do
  e' <- applyE s e

  pure $ MLIR.MkExprReturn e'

applyD
  :: MonadIO m
  => Map MLIR.QuVar MLIR.PlumeType
  -> MLIR.MLIR "declaration"
  -> m (MLIR.MLIR "declaration")
applyD subst (MLIR.MkDeclFunction name gens args ret body) = do
  args' <- mapM (traverse (M.apply subst)) args
  ret' <- M.apply subst ret
  body' <- applyE subst body

  pure $ MLIR.MkDeclFunction name gens args' ret' body'
applyD subst (MLIR.MkDeclVariable name gens expr) = do
  name' <- traverse (M.apply subst) name
  gens' <- traverse (M.apply subst) gens
  expr' <- applyE subst expr

  pure $ MLIR.MkDeclVariable name' gens' expr'
applyD _ d@(MLIR.MkDeclNative {}) = pure d

insertAfter
  :: Text
  -> [MLIR.MLIR "declaration"]
  -> MLIR.MLIR "declaration"
  -> [MLIR.MLIR "declaration"]
insertAfter name (d@(MLIR.MkDeclFunction fnName _ _ _ _) : xs) decl
  | name == fnName = do
    d : decl : xs
  | otherwise = d : insertAfter name xs decl
insertAfter name (d@(MLIR.MkDeclVariable var _ _) : xs) decl
  | name == var.name = do
    d : decl : xs
  | otherwise = d : insertAfter name xs decl
insertAfter name (x:xs) decl = x : insertAfter name xs decl
insertAfter _ [] decl = [decl]

getBody :: Text -> [MLIR.MLIR "declaration"] -> Maybe (MLIR.MLIR "declaration")
getBody name (d@(MLIR.MkDeclFunction fnName _ _ _ _) : xs)
  | name == fnName = Just d
  | otherwise = getBody name xs
getBody name (d@(MLIR.MkDeclVariable var _ _) : xs)
  | name == var.name = Just d
  | otherwise = getBody name xs
getBody name (_:xs) = getBody name xs
getBody _ [] = Nothing

replaceNameWith :: Text -> Text -> MLIR.MLIR "declaration" -> MLIR.MLIR "declaration"
replaceNameWith old new (MLIR.MkDeclFunction name gens args ret body)
  | name == old = MLIR.MkDeclFunction new gens args ret body
  | otherwise = MLIR.MkDeclFunction name gens args ret body
replaceNameWith old new (MLIR.MkDeclVariable name gens expr)
  | name.name == old = MLIR.MkDeclVariable (MLIR.MkAnnotation new name.value) gens expr
  | otherwise = MLIR.MkDeclVariable name gens expr
replaceNameWith _ _ d = d

monomorphize
  :: M.MonadMono m
  => [MLIR.MLIR "declaration"]
  -> m [MLIR.MLIR "declaration"]
monomorphize decls = do
  mapM_ convertMono decls

  readIORef M.resultState
