{-# LANGUAGE LambdaCase #-}
module Language.Plume.Backend.LLIR.Conversion where

import Language.Plume.Syntax.LLIR qualified as LLIR
import Language.Plume.Syntax.MLIR qualified as MLIR
import Language.Plume.Backend.LLIR.Monad qualified as M
import Data.Map qualified as Map
import Data.Set qualified as Set
import Control.Monad.Result (compilerError)

convert
  :: M.MonadLLIR m
  => MLIR.MLIR "declaration"
  -> m ()
convert (MLIR.MkDeclFunction name gens args _ body) = do
  (body', ret) <- convertE body

  let ty = map (.value) args LLIR.:->: ret
  modifyIORef M.globals (Map.insert name ty)

  modifyIORef' M.resultState (<> [LLIR.MkDeclFunction name gens args ret body'])
convert (MLIR.MkDeclVariable ann _ expr) = do
  (expr', ty) <- convertE expr

  modifyIORef' M.globalVariables (<> Set.singleton ann.name)
  modifyIORef' M.globals (Map.insert ann.name ty)

  let fun = LLIR.MkDeclFunction ann.name [] [] ty expr'

  modifyIORef' M.resultState (<> [fun])
convert (MLIR.MkDeclNative name gens args ret) = do
  modifyIORef' M.resultState (<> [LLIR.MkDeclNative name gens args ret])
convert (MLIR.MkDeclExtend {}) = compilerError "TODO"

findStruct :: M.MonadLLIR m => Text -> m [LLIR.Annotation LLIR.PlumeType]
findStruct name = do
  decls <- readIORef M.resultState

  case filter (\case LLIR.MkDeclStruct name' _ -> name == name'; _ -> False) decls of
    [LLIR.MkDeclStruct _ fields] -> pure fields
    _ -> compilerError "Struct not found."

typeOfLit :: MLIR.Literal -> LLIR.PlumeType
typeOfLit (MLIR.MkInteger _) = LLIR.MkTyInt
typeOfLit (MLIR.MkFloat _) = LLIR.MkTyFloat
typeOfLit (MLIR.MkChar _) = LLIR.MkTyChar
typeOfLit (MLIR.MkString _) = LLIR.MkTyString
typeOfLit (MLIR.MkBool _) = LLIR.MkTyBool

convertE
  :: M.MonadLLIR m
  => MLIR.MLIR "expression"
  -> m (LLIR.LLIR "expression", LLIR.PlumeType)
convertE (MLIR.MkExprLiteral lit) =
  pure (LLIR.MkExprLiteral lit, typeOfLit lit)
convertE (MLIR.MkExprVariable name ty) = do
  globals <- readIORef M.globals
  locals <- readIORef M.locals
  globalVars <- readIORef M.globalVariables

  case Map.lookup name globals of
    Just ty' | name `Map.notMember` locals -> if Set.member name globalVars 
      then do
        let funTy = [] LLIR.:->: ty'
        pure (LLIR.MkExprCall (LLIR.MkExprVariable name funTy) [] funTy, ty')
      else pure (LLIR.MkExprVariable name ty', ty')
    _ -> case Map.lookup name locals of
      Just ty' -> do
        pure (LLIR.MkExprVariable name ty', ty')
      _ -> pure (LLIR.MkExprVariable name ty, ty)
convertE (MLIR.MkExprCall callee args _) = do
  (callee', ty) <- convertE callee
  (args', _) <- mapAndUnzipM convertE args
  
  case ty of
    _ MLIR.:->: ret -> 
      pure (LLIR.MkExprCall callee' args' ty, ret)
    _ -> compilerError "Call is not well-typed."
convertE (MLIR.MkExprLet ann _ expr _ body) = do
  (expr', ty) <- convertE expr

  modifyIORef M.locals (Map.insert ann ty)
  (body', bTy) <- case body of
    Just body' -> convertE body' >>= \(x, t) -> pure (Just x, t)
    Nothing -> pure (Nothing, LLIR.MkTyUnit)

  pure (LLIR.MkExprLet ann ty expr' body', bTy)
convertE (MLIR.MkExprIf cond then' else') = do
  (cond', _) <- convertE cond
  (then'', t1) <- convertE then'
  (else'', _) <- convertE else'

  pure (LLIR.MkExprIf cond' then'' else'' t1, t1)
convertE (MLIR.MkExprLambda args _ body) = do
  (body', ret) <- convertE body

  newFunName <- M.freshSymbol "lambda"

  let newFun = LLIR.MkDeclFunction newFunName [] args ret body'

  modifyIORef' M.resultState (<> [newFun])

  let funTy = map (.value) args LLIR.:->: ret

  pure (LLIR.MkExprVariable newFunName funTy, funTy)
convertE (MLIR.MkExprBlock es t) = do
  old <- readIORef M.locals
  (es', _) <- mapAndUnzipM convertE es
  writeIORef M.locals old

  pure (LLIR.MkExprBlock es', t)
convertE (MLIR.MkExprField e name t) = do
  (e', _) <- convertE e

  pure (LLIR.MkExprPtrField e' name t, t)
convertE (MLIR.MkExprTupleAccess e i t) = do
  (e', ty) <- convertE e

  annots <- case ty of
    LLIR.MkTyId name -> findStruct name
    t' -> compilerError $ "TupleAccess is not well-typed. Received: " <> show t'

  let field
        | i == 0 = "closure"
        | i == 1 = "env"
        | otherwise = compilerError "TupleAccess is not well-typed."

  case maybeAt i annots of
    Just (LLIR.MkAnnotation _ finalTy) -> pure (LLIR.MkExprField e' field ty finalTy, t)
    Nothing -> compilerError "TupleAccess is not well-typed."
convertE (MLIR.MkExprPack envTy (lam, env) (LLIR.MkTyTuple [funTy, _])) = do
  envTyName <- M.freshSymbol "env"
  let stct = LLIR.MkDeclStruct envTyName envTy

  closStruct <- M.freshSymbol "closure"
  let clos = LLIR.MkDeclStruct closStruct [LLIR.MkAnnotation "closure" funTy, LLIR.MkAnnotation "env" (LLIR.MkTyId envTyName)]

  modifyIORef' M.resultState (<> [stct, clos])
  (lambdaVar, _) <- convertE lam
  env' <- forM env $ \(LLIR.MkAnnotation name e) -> do
    (e', _) <- convertE e
    pure $ LLIR.MkAnnotation name e'

  let envExpr = LLIR.MkExprStruct envTyName env'
  return (LLIR.MkExprStruct closStruct [LLIR.MkAnnotation "closure" lambdaVar, LLIR.MkAnnotation "env" envExpr], LLIR.MkTyId closStruct)
convertE (MLIR.MkExprClosureCall callee (env:args) t) = do
  (callee', ct) <- convertE callee
  (env', ty) <- convertE env
  (args', _) <- mapAndUnzipM convertE args

  let newEnv = LLIR.MkExprCast (LLIR.MkExprRef env' ty) LLIR.MkTyAny

  pure (LLIR.MkExprCall callee' (newEnv:args') t, ct)
convertE (MLIR.MkExprPack {}) =
  compilerError "Pack is not well-typed."
convertE (MLIR.MkExprClosureCall {}) =
  compilerError "ClosureCall is not well-typed."
convertE (MLIR.MkExprReturn e) = do
  (e', ty) <- convertE e

  pure (LLIR.MkExprReturn e', ty)
convertE (MLIR.MkExprLocated _ e) =
  convertE e

convertToLLIR
  :: MonadIO m
  => [MLIR.MLIR "declaration"]
  -> m [LLIR.LLIR "declaration"]
convertToLLIR decls = do
  mapM_ convert decls

  readIORef M.resultState
