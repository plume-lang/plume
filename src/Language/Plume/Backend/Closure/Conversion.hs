module Language.Plume.Backend.Closure.Conversion where

import Language.Plume.Syntax.MLIR qualified as MLIR
import Language.Plume.Backend.Closure.Monad qualified as C
import Data.Map qualified as Map
import Data.Set qualified as Set

isBlock :: MLIR.MLIR "expression" -> Bool
isBlock (MLIR.MkExprBlock {}) = True
isBlock _ = False

convertClosure
  :: C.MonadClosure m
  => MLIR.MLIR "expression"
  -> m (MLIR.MLIR "expression", MLIR.PlumeType)
convertClosure (MLIR.MkExprLambda args _ body) = do
  (body', ty) <- convertClosure body

  let argsTys = map (.value) args

  envName <- C.freshSymbol "env"
  var <- newIORef (MLIR.Unbound envName 0)
  let envTy = MLIR.MkTyVar var

  let closTy = MLIR.MkTyExists envName (MLIR.MkTyTuple [
          (envTy : argsTys) MLIR.:->: ty
        , envTy
        ])

  let freeVars = C.free body' Map.\\ C.free args
  let annots = Map.mapWithKey MLIR.MkAnnotation freeVars

  let envVar = MLIR.MkExprVariable "env" envTy

  let decls = Map.mapWithKey (\name ty' -> MLIR.MkExprLet name ty' (MLIR.MkExprField envVar name ty') MLIR.MkTyUnit Nothing) freeVars
  let decls' = Map.elems decls

  let block 
        | null decls' = body'
        | isBlock body' = MLIR.MkExprBlock (decls' ++ [body']) ty
        | otherwise = MLIR.MkExprBlock (decls' ++ [MLIR.MkExprReturn body']) ty

  let envArg = MLIR.MkAnnotation "env" envTy
  let lambda = MLIR.MkExprLambda (envArg:args) ty block

  let fields = map (\(MLIR.MkAnnotation name varTy) -> MLIR.MkAnnotation name (MLIR.MkExprVariable name varTy)) $ Map.elems annots

  pure (MLIR.MkExprPack (Map.elems annots) (lambda, fields) closTy, closTy)

convertClosure (MLIR.MkExprCall callee args t) = do
  (callee', ty) <- convertClosure callee
  (args', _) <- mapAndUnzipM convertClosure args

  reserved <- readIORef C.reserved

  case callee' of
    MLIR.MkExprVariable varName _ | varName `Set.member` reserved -> do
      pure (MLIR.MkExprCall callee' args' t, ty)
    _ ->
      case ty of
        (tyArgs MLIR.:->: tyRet) -> do
          t_ve <- C.freshType "type"
          f_name <- C.freshSymbol "closure"

          let newFunTy = (t_ve:tyArgs) MLIR.:->: tyRet
          let tpl = MLIR.MkTyTuple [newFunTy, t_ve]

          let decl = MLIR.MkExprLet f_name tpl callee' tyRet . Just
              var = MLIR.MkExprVariable f_name tpl

          let newCallee = MLIR.MkExprTupleAccess var 0 newFunTy
              newEnv = MLIR.MkExprTupleAccess var 1 t_ve
              newArgs = newEnv:args'

          pure (decl $ MLIR.MkExprClosureCall newCallee newArgs t, tyRet)
        MLIR.MkTyExists _ (MLIR.MkTyTuple [tyArgs@(t_ve:_) MLIR.:->: tyRet, _]) -> do
          f_name <- C.freshSymbol "closure"

          let newFunTy = tyArgs MLIR.:->: tyRet
          let tpl = MLIR.MkTyTuple [newFunTy, t_ve]

          let decl = MLIR.MkExprLet f_name tpl callee' tyRet . Just
              var = MLIR.MkExprVariable f_name tpl

          let newCallee = MLIR.MkExprTupleAccess var 0 newFunTy
              newEnv = MLIR.MkExprTupleAccess var 1 t_ve
              newArgs = newEnv:args'

          pure (decl $ MLIR.MkExprClosureCall newCallee newArgs t, tyRet)
        t' -> error $ "expected function type, received: " <> show t'
convertClosure (MLIR.MkExprBlock exprs t) = do
  (exprs', _) <- convertClosureMany exprs

  pure (MLIR.MkExprBlock exprs' t, MLIR.MkTyUnit)
convertClosure (MLIR.MkExprIf cond then' else') = do
  (cond', _) <- convertClosure cond
  (then'', t2) <- convertClosure then'
  (else'', _) <- convertClosure else'

  pure (MLIR.MkExprIf cond' then'' else'', t2)
convertClosure (MLIR.MkExprLiteral l) = pure (MLIR.MkExprLiteral l, MLIR.MkTyInt)
convertClosure (MLIR.MkExprVariable name ty) = do
  globals <- readIORef C.globals

  case (Map.lookup name globals, ty) of
    (Just (MLIR.MkTyExists _ _), argsTys MLIR.:->: ret) -> do
      envName <- C.freshSymbol "env"
      var <- newIORef (MLIR.Unbound envName 0)
      let envTy = MLIR.MkTyVar var
      let closTy = MLIR.MkTyExists envName (MLIR.MkTyTuple [
              (envTy : argsTys) MLIR.:->: ret
            , envTy
            ])

      pure (MLIR.MkExprVariable name closTy, closTy)
    _ -> pure (MLIR.MkExprVariable name ty, ty)
convertClosure (MLIR.MkExprField expr field t) = do
  (expr', ty) <- convertClosure expr

  pure (MLIR.MkExprField expr' field t, ty)
convertClosure (MLIR.MkExprPack {}) = error "TODO"
convertClosure (MLIR.MkExprLet {}) = error "TODO"
convertClosure (MLIR.MkExprTupleAccess {}) = error "todo"
convertClosure (MLIR.MkExprClosureCall {}) = error "todo"
convertClosure (MLIR.MkExprReturn expr) = do
  (expr', ty) <- convertClosure expr

  pure (MLIR.MkExprReturn expr', ty)

convertClosureD
  :: C.MonadClosure m
  => MLIR.MLIR "declaration"
  -> m [MLIR.MLIR "declaration"]
convertClosureD (MLIR.MkDeclFunction name generics args _ body) = do
  modifyIORef C.reserved (Set.insert name)
  (body', t) <- convertClosure body

  modifyIORef C.globals (Map.insert name t)

  pure [MLIR.MkDeclFunction name generics args t body']
convertClosureD (MLIR.MkDeclVariable name qvs expr) = do
  (expr', t) <- convertClosure expr

  modifyIORef C.globals (Map.insert name.name t)

  pure  [MLIR.MkDeclVariable (t <$ name) qvs expr']
convertClosureD (MLIR.MkDeclNative name gens args ret) = do
  modifyIORef C.reserved (Set.insert name)
  pure [MLIR.MkDeclNative name gens args ret]

convertClosureMany
  :: C.MonadClosure m
  => [MLIR.MLIR "expression"]
  -> m ([MLIR.MLIR "expression"], [MLIR.PlumeType])
convertClosureMany xs = do
  (exprs, tys) <- mapAndUnzipM convertClosure xs

  pure (exprs, tys)

mapAndUnzip3M :: (Monad m) => (a -> m (b, c, d)) -> [a] -> m ([b], [c], [d])
mapAndUnzip3M f xs = unzip3 <$> mapM f xs

runClosureConversion
  :: MonadIO m
  => [MLIR.MLIR "declaration"]
  -> m [MLIR.MLIR "declaration"]
runClosureConversion decls = do
  writeIORef C.reserved $ Set.fromList ["*", "+", "/", "-", ">", "<", "==", "!=", "unreachable"]
  decls' <- mapM convertClosureD decls

  pure (concat decls')
