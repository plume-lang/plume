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

{-# NOINLINE globalVars #-}
globalVars :: IORef (S.Set Text)
globalVars = unsafePerformIO $ newIORef S.empty

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
  :: (MonadClosure m)
  => [Text]
  -> Post.ClosedStatement
  -> m ([Post.ClosedProgram], Post.ClosedExpr)
closeClosure args e = do
  globalV <- readIORef globalVars
  let env = free e S.\\ (S.fromList args <> globalV)

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
        Post.CSExpr (Post.CEBlock es) -> Post.CSExpr (Post.CEBlock (envDecl <> es))
        _ -> case envDecl of
          [] -> substBody
          _ -> Post.CSExpr (Post.CEBlock (envDecl <> [substBody]))

  let lambdaDict =
        Post.CEDictionary $
          fromList [(0, envDict), (1, Post.CEVar name)]

  return
    ([Post.CPFunction name (name <> "_env" : args) body], lambdaDict)

closeExpression
  :: (MonadClosure m)
  => Pre.TypedExpression PlumeType
  -> m ([Post.ClosedProgram], Post.ClosedExpr)
closeExpression (Pre.EVariable x _) = pure ([], Post.CEVar x)
closeExpression (Pre.EExtVariable x _) = pure ([], Post.CEVar x)
closeExpression (Pre.ELiteral l) = pure ([], Post.CELiteral l)
closeExpression (Pre.EList es) = (Post.CEList <$>) . sequence <$> traverse closeExpression es
closeExpression (Pre.EApplication f args) = do
  res <- readIORef reserved
  case f of
    Pre.EVariable x _ | x `S.member` res -> do
      (prog, args') <- sequence <$> traverse closeExpression args
      pure (prog, Post.CEApplication (Post.CEVar x) args')
    _ -> do
      name <- newCallName
      (p1, f') <- closeExpression f
      (p2s, args') <- sequence <$> traverse closeExpression args
      let callVar = Post.CEVar name
      let callVarP = Post.CEProperty callVar 1
      let callDict = Post.CEProperty callVar 0
      let call = Post.CEApplication callVarP (callDict : args')
      pure (p1 <> p2s, Post.CEDeclaration name f' call)
closeExpression (Pre.EDeclaration (Annotation name _) _ e1 (Just e2)) = do
  (p1, e1') <- closeExpression e1
  (p2, e2') <- closeExpression e2
  pure (p1 <> p2, Post.CEDeclaration name e1' e2')
closeExpression (Pre.EDeclaration (Annotation n _) _ _ Nothing) =
  error n
closeExpression (Pre.EConditionBranch e1 e2 e3) = do
  (p1, e1') <- closeExpression e1
  (p2, e2') <- closeExpression e2
  (p3, e3') <- case e3 of
    Just e3' -> closeExpression e3'
    Nothing -> error "TODO: Implement this."
  pure (p1 <> p2 <> p3, Post.CEConditionBranch e1' e2' e3')
closeExpression (Pre.EClosure args _ e) = do
  let args' = map (.annotationName) args
  (stmts, body) <- case removeLocated e of
    b@(Pre.EBlock _) -> closeStatement b
    e' -> (Post.CSReturn <$>) <$> closeExpression e'
  first (stmts <>) <$> closeClosure args' body
closeExpression (Pre.EBlock es) = (Post.CEBlock <$>) . sequence <$> traverse closeStatement es
closeExpression (Pre.ESwitch e cases) = do
  (s1, e') <- closeExpression e
  (s2, cases') <-
    sequence
      <$> traverse
        ( \(p, body) -> do
            pat <- closePattern p
            (stmts, b) <- closeExpression body
            return (stmts, (pat, b))
        )
        cases
  pure (s1 <> s2, Post.CESwitch e' cases')
closeExpression (Pre.EReturn e) = closeExpression e
closeExpression (Pre.ELocated e _) = closeExpression e
closeExpression (Pre.EExtensionDeclaration {}) =
  error "Should not encounter extension declaration in closure conversion."
closeExpression (Pre.ENativeFunction {}) = error "Should not encounter native function in closure conversion."

closePattern
  :: (MonadClosure m) => Pre.TypedPattern t -> m Post.ClosedPattern
closePattern (Pre.PVariable x _) = pure $ Post.CPVariable x
closePattern (Pre.PLiteral l) = pure $ Post.CPLiteral l
closePattern (Pre.PConstructor name ps) = do
  ps' <- traverse closePattern ps
  pure $ Post.CPConstructor name ps'
closePattern Pre.PWildcard = pure Post.CPWildcard

closeStatement
  :: (MonadClosure m)
  => Pre.TypedExpression PlumeType
  -> m ([Post.ClosedProgram], Post.ClosedStatement)
closeStatement (Pre.EReturn e) = (Post.CSReturn <$>) <$> closeExpression e
closeStatement (Pre.EDeclaration (Annotation name _) _ e1 Nothing) = do
  (stmts, e1') <- closeExpression e1
  pure (stmts, Post.CSDeclaration name e1')
closeStatement (Pre.ELocated e _) = closeStatement e
closeStatement (Pre.EConditionBranch e1 e2 e3) = do
  (p1, e1') <- closeExpression e1
  (p2, e2') <- closeStatement e2
  (p3, e3') <-
    maybe (return ([], Post.CSExpr (Post.CEBlock []))) closeStatement e3
  pure (p1 <> p2 <> p3, Post.CSConditionBranch e1' e2' e3')
closeStatement e = (Post.CSExpr <$>) <$> closeExpression e

closeProgram
  :: (MonadClosure m) => Pre.TypedExpression PlumeType -> m [Post.ClosedProgram]
closeProgram (Pre.EDeclaration (Annotation name _) _ (Pre.EClosure args _ e) _) = do
  let args' = map (.annotationName) args
  modifyIORef' reserved (S.insert name)
  (stmts, e') <- closeStatement e
  pure $ stmts ++ [Post.CPFunction name args' e']
closeProgram (Pre.ELocated e _) = closeProgram e
closeProgram (Pre.EExtensionDeclaration (Annotation name _) ty _ arg e) = do
  (stmts, e') <- closeStatement e
  pure $ stmts ++ [Post.CPExtFunction ty name arg.annotationName e']
closeProgram (Pre.ENativeFunction name _ (args :->: _)) = do
  modifyIORef' reserved (S.insert name)
  pure [Post.CPNativeFunction name (length args)]
closeProgram e = do
  (stmts, s) <- closeStatement e
  pure $ stmts ++ [Post.CPStatement s]

runClosureConversion
  :: (MonadIO m)
  => [Pre.TypedExpression PlumeType]
  -> m (Either Text [Post.ClosedProgram])
runClosureConversion e = do
  writeIORef closedCounter 0
  (concat <$>) <$> runExceptT (traverse (closeProgram . removeLocated) e)

removeLocated :: Pre.TypedExpression t -> Pre.TypedExpression t
removeLocated (Pre.ELocated e _) = removeLocated e
removeLocated e = e