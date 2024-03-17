{-# LANGUAGE OverloadedRecordDot #-}

module Plume.Compiler.ClosureConversion.Conversion where

import Control.Monad.Except
import Data.Map qualified as M
import Data.Set qualified as S
import GHC.IO
import Plume.Compiler.ClosureConversion.Free
import Plume.Compiler.ClosureConversion.Syntax qualified as Post
import Plume.Syntax.Common.Annotation
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR qualified as Pre

type MonadClosure m = (MonadIO m, MonadError Text m)

{-# NOINLINE closedFunctions #-}
closedFunctions :: IORef [Post.ClosedProgram]
closedFunctions = unsafePerformIO $ newIORef []

{-# NOINLINE closedCounter #-}
closedCounter :: IORef Int
closedCounter = unsafePerformIO $ newIORef 0

{-# NOINLINE callCounter #-}
callCounter :: IORef Int
callCounter = unsafePerformIO $ newIORef 0

newLambdaName :: (MonadIO m) => m Text
newLambdaName = do
  i <- readIORef closedCounter
  writeIORef closedCounter (i + 1)
  pure $ "lambda" <> show i

newCallName :: (MonadIO m) => m Text
newCallName = do
  i <- readIORef callCounter
  writeIORef callCounter (i + 1)
  pure $ "call" <> show i

{-# NOINLINE reserved #-}
reserved :: IORef (S.Set Text)
reserved = unsafePerformIO $ newIORef S.empty

closeClosure
  :: (MonadClosure m) => [Text] -> Post.ClosedStatement -> m Post.ClosedExpr
closeClosure args e = do
  let env = free e S.\\ S.fromList args

  name <- newLambdaName

  let envVars = fromList $ zip (S.toList env) [0 ..]
  let envDict =
        Post.CEDictionary . fromList $
          map (\(x, i) -> (i, Post.CEVar x)) $
            M.toList envVars

  let envVar = Post.CEVar $ name <> "_env"
  let envProps = M.map (Post.CEProperty envVar) envVars

  let envDecl =
        map (\(x, i) -> Post.CSDeclaration x (Post.CEProperty envVar i)) $
          M.toList envVars

  let substBody = substituteMany (M.toList envProps) e

  let body = case substBody of
        Post.CSBlock es -> Post.CSBlock (envDecl <> es)
        _ -> Post.CSBlock (envDecl <> [e])

  let lambdaDict =
        Post.CEDictionary $
          fromList [(0, envDict), (1, Post.CEVar name)]

  modifyIORef'
    closedFunctions
    (Post.CPFunction name (name <> "_env" : args) body :)

  return lambdaDict

closeExpression
  :: (MonadClosure m) => Pre.TypedExpression PlumeType -> m Post.ClosedExpr
closeExpression (Pre.EVariable x _) = pure $ Post.CEVar x
closeExpression (Pre.EExtVariable x _) = pure $ Post.CEVar x
closeExpression (Pre.ELiteral l) = pure $ Post.CELiteral l
closeExpression (Pre.EList es) = Post.CEList <$> traverse closeExpression es
closeExpression (Pre.EApplication f args) = do
  res <- readIORef reserved
  case f of
    Pre.EVariable x _ | x `S.member` res -> do
      args' <- traverse closeExpression args
      pure $ Post.CEApplication (Post.CEVar x) args'
    _ -> do
      name <- newCallName
      f' <- closeExpression f
      args' <- traverse closeExpression args
      let callVar = Post.CEProperty f' 1
      let callDict = Post.CEProperty f' 0
      let call = Post.CEApplication callVar (callDict : args')
      pure $ Post.CEDeclaration name f' call
closeExpression (Pre.EDeclaration (Annotation name _) _ e1 (Just e2)) = do
  e1' <- closeExpression e1
  e2' <- closeExpression e2
  pure $ Post.CEDeclaration name e1' e2'
closeExpression (Pre.EDeclaration (Annotation n _) _ _ Nothing) =
  error n
closeExpression (Pre.EConditionBranch e1 e2 e3) = do
  e1' <- closeExpression e1
  e2' <- closeStatement e2
  e3' <- case e3 of
    Just e3' -> closeStatement e3'
    Nothing -> error "TODO: Implement this."
  pure $ Post.CEConditionBranch e1' e2' e3'
closeExpression (Pre.EClosure args _ e) = do
  let args' = map (.annotationName) args
  closeClosure args' =<< closeStatement e
closeExpression (Pre.EBlock _) =
  error "Should not encounter block in closure conversion."
closeExpression (Pre.ESwitch e cases) = do
  e' <- closeExpression e
  cases' <-
    traverse (\(p, body) -> (,) <$> closePattern p <*> closeStatement body) cases
  pure $ Post.CESwitch e' cases'
closeExpression (Pre.EReturn e) = closeExpression e
closeExpression (Pre.ENativeFunction name _ _) = pure $ Post.CENativeFunction name 0
closeExpression (Pre.ELocated e _) = closeExpression e
closeExpression (Pre.EExtensionDeclaration {}) =
  error "Should not encounter extension declaration in closure conversion."

closePattern
  :: (MonadClosure m) => Pre.TypedPattern t -> m Post.ClosedPattern
closePattern (Pre.PVariable x _) = pure $ Post.CPVariable x
closePattern (Pre.PLiteral l) = pure $ Post.CPLiteral l
closePattern (Pre.PConstructor name ps) = do
  ps' <- traverse closePattern ps
  pure $ Post.CPConstructor name ps'
closePattern Pre.PWildcard = pure Post.CPWildcard

closeStatement
  :: (MonadClosure m) => Pre.TypedExpression PlumeType -> m Post.ClosedStatement
closeStatement (Pre.EReturn e) = Post.CSReturn <$> closeExpression e
closeStatement (Pre.EBlock es) = Post.CSBlock <$> traverse closeStatement es
closeStatement (Pre.EDeclaration (Annotation name _) _ e1 Nothing) = do
  e1' <- closeExpression e1
  pure $ Post.CSDeclaration name e1'
closeStatement (Pre.ELocated e _) = closeStatement e
closeStatement e = Post.CSExpr <$> closeExpression e

closeProgram
  :: (MonadClosure m) => Pre.TypedExpression PlumeType -> m Post.ClosedProgram
closeProgram (Pre.EDeclaration (Annotation name _) _ (Pre.EClosure args _ e) _) = do
  let args' = map (.annotationName) args
  e' <- closeStatement e
  pure $ Post.CPFunction name args' e'
closeProgram (Pre.ELocated e _) = closeProgram e
closeProgram (Pre.EExtensionDeclaration (Annotation name _) ty _ args e) = do
  e' <- closeStatement e
  pure $ Post.CPExtFunction ty name (map (.annotationName) args) e'
closeProgram e = Post.CPStatement <$> closeStatement e

runClosureConversion
  :: (MonadIO m)
  => [Pre.TypedExpression PlumeType]
  -> m (Either Text [Post.ClosedProgram])
runClosureConversion e = do
  writeIORef closedFunctions []
  writeIORef closedCounter 0
  runExceptT $ traverse closeProgram e