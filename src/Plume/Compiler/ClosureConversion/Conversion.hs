module Plume.Compiler.ClosureConversion.Conversion where

import Control.Monad.Except
import Data.Map qualified as M
import Data.Set qualified as S
import GHC.IO
import Plume.Compiler.ClosureConversion.Free
import Plume.Compiler.ClosureConversion.Syntax qualified as Post
import Plume.Compiler.TypeErasure.Syntax qualified as Pre

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
  res <- readIORef reserved
  let env = free e S.\\ (S.fromList args <> globalV <> res)

  name <- newLambdaName
  modifyIORef' globalVars (<> S.singleton name)

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
        Post.CSExpr (Post.CEBlock [Post.CSExpr ret]) ->
          Post.CSExpr (Post.CEBlock (envDecl <> [Post.CSReturn ret]))
        Post.CSExpr (Post.CEBlock es) ->
          Post.CSExpr (Post.CEBlock (envDecl <> es))
        Post.CSExpr ret -> Post.CSExpr (Post.CEBlock (envDecl <> [Post.CSReturn ret]))
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
  => Pre.UntypedExpr
  -> m ([Post.ClosedProgram], Post.ClosedExpr)
closeExpression (Pre.UEVar x) = pure ([], Post.CEVar x)
closeExpression (Pre.UELiteral l) = pure ([], Post.CELiteral l)
closeExpression (Pre.UEList es) = (Post.CEList <$>) . sequence <$> traverse closeExpression es
closeExpression Pre.UESpecial = pure ([], Post.CESpecial)
closeExpression (Pre.UEApplication f args) = do
  res <- readIORef reserved
  gv <- readIORef globalVars
  (p2s, args') <- sequence <$> traverse closeExpression args
  case f of
    Pre.UEVar x | x `S.member` (res <> gv) -> do
      pure (p2s, Post.CEApplication (Post.CEVar x) args')
    _ -> do
      name <- newCallName
      (p1, f') <- closeExpression f
      let callVar = Post.CEVar name
      let callVarP = Post.CEProperty callVar 1
      let callDict = Post.CEProperty callVar 0
      let call = Post.CEApplication callVarP (callDict : args')
      pure (p1 <> p2s, Post.CEDeclaration name f' call)
closeExpression (Pre.UEDeclaration name e1 e2) = do
  (p1, e1') <- closeExpression e1
  (p2, e2') <- closeExpression e2
  pure (p1 <> p2, Post.CEDeclaration name e1' e2')
closeExpression (Pre.UEConditionBranch e1 e2 e3) = do
  (p1, e1') <- closeExpression e1
  (p2, e2') <- closeExpression e2
  (p3, e3') <- closeExpression e3
  pure (p1 <> p2 <> p3, Post.CEConditionBranch e1' e2' e3')
closeExpression (Pre.UEClosure args e) = do
  (stmts, body) <- closeStatement e
  first (stmts <>) <$> closeClosure args body
closeExpression (Pre.UEBlock es) = (Post.CEBlock <$>) . sequence <$> traverse closeStatement es
closeExpression (Pre.UESwitch e cases) = do
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
closeExpression (Pre.UEEqualsType e t) = do
  (stmts, e') <- closeExpression e
  pure (stmts, Post.CEEqualsType e' t)
closeExpression (Pre.UEAnd e1 e2) = do
  (p1, e1') <- closeExpression e1
  (p2, e2') <- closeExpression e2
  pure (p1 <> p2, Post.CEAnd e1' e2')
closeExpression (Pre.UEIndex e1 e2) = do
  (p1, e1') <- closeExpression e1
  (p2, e2') <- closeExpression e2
  pure (p1 <> p2, Post.CEIndex e1' e2')

closePattern
  :: (MonadClosure m) => Pre.UntypedPattern -> m Post.ClosedPattern
closePattern (Pre.UPVariable x) = pure $ Post.CPVariable x
closePattern (Pre.UPLiteral l) = pure $ Post.CPLiteral l
closePattern (Pre.UPConstructor name ps) = do
  ps' <- traverse closePattern ps
  pure $ Post.CPConstructor name ps'
closePattern Pre.UPWildcard = pure Post.CPWildcard
closePattern (Pre.UPSpecialVariable x) = pure $ Post.CPSpecialVar x

closeStatement
  :: (MonadClosure m)
  => Pre.UntypedStatement
  -> m ([Post.ClosedProgram], Post.ClosedStatement)
closeStatement (Pre.USReturn e) = (Post.CSReturn <$>) <$> closeExpression e
closeStatement (Pre.USDeclaration name e1) = do
  (stmts, e1') <- closeExpression e1
  pure (stmts, Post.CSDeclaration name e1')
closeStatement (Pre.USConditionBranch e1 e2 e3) = do
  (p1, e1') <- closeExpression e1
  (p2, e2') <- closeStatement e2
  (p3, e3') <- closeStatement e3
  pure (p1 <> p2 <> p3, Post.CSConditionBranch e1' e2' e3')
closeStatement (Pre.USExpr e) = do
  (stmts, e') <- closeExpression e
  pure (stmts, Post.CSExpr e')

makeReturn :: Post.ClosedExpr -> [Post.ClosedStatement]
makeReturn (Post.CEBlock es) = es
makeReturn e = [Post.CSReturn e]

makeReturnStmt :: Post.ClosedStatement -> Post.ClosedStatement
makeReturnStmt e@(Post.CSExpr (Post.CEBlock _)) = e
makeReturnStmt (Post.CSExpr e) = Post.CSReturn e
makeReturnStmt e = e

makeReturnBody :: Post.ClosedExpr -> Post.ClosedStatement
makeReturnBody e = Post.CSExpr (Post.CEBlock $ makeReturn e)

closeProgram
  :: (MonadClosure m) => Pre.UntypedProgram -> m [Post.ClosedProgram]
closeProgram (Pre.UPFunction name args e) = do
  modifyIORef' globalVars (<> S.singleton name)
  (stmts, e') <- closeStatement e
  pure $ stmts ++ [Post.CPFunction name args (makeReturnStmt e')]
closeProgram (Pre.UPNativeFunction fp name arity) = do
  modifyIORef' reserved (S.insert name)
  pure [Post.CPNativeFunction fp name arity]
closeProgram (Pre.UPStatement s) = do
  (stmts, s') <- closeStatement s
  pure $ stmts ++ [Post.CPStatement s']

runClosureConversion
  :: (MonadIO m)
  => [Pre.UntypedProgram]
  -> m (Either Text [Post.ClosedProgram])
runClosureConversion e = do
  writeIORef closedCounter 0
  (concat <$>) <$> runExceptT (traverse closeProgram e)
