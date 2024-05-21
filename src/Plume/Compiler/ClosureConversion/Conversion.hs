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

{-# NOINLINE functions #-}
functions :: IORef (M.Map Text Int)
functions = unsafePerformIO $ newIORef M.empty

{-# NOINLINE locals #-}
locals :: IORef (S.Set Text)
locals = unsafePerformIO $ newIORef S.empty

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
  pure $ "@lambda" <> show i

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
  funs <- S.fromList . M.keys <$> readIORef functions
  lcls <- readIORef locals

  let defined = (funs <> globalV) S.\\ lcls

  let env = free e S.\\ (S.fromList args <> defined)

  name <- newLambdaName
  modifyIORef' functions (M.insert name (length args))

  let envVars = S.toList env
      envDict =
        Post.CEDictionary . fromList $
          map (\x -> (x, Post.CEVar x)) envVars

  let envVar = Post.CEVar $ name <> "_env"
  let envProps = map (\x -> (x, Post.CEProperty envVar x)) envVars

  let envDecl =
        map (\x -> Post.CSDeclaration x (Post.CEProperty envVar x)) envVars

  let substBody = substituteMany envProps e

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
          fromList [("env", envDict), ("f", Post.CEVar name)]

  return
    ([Post.CPFunction name (name <> "_env" : args) body], lambdaDict)

closeExpression
  :: (MonadClosure m)
  => Pre.UntypedExpr
  -> m ([Post.ClosedProgram], Post.ClosedExpr)
closeExpression (Pre.UEVar x) = do
  res <- readIORef reserved
  lcls <- getDefinedWithoutNatives
  if x `S.member` lcls 
    then pure ([], Post.CEVar x)
  else if x `S.member` res
    then do
      funs <- readIORef functions
      case M.lookup x funs of
        Just arity -> do
          let args = [x <> "_arg" <> show i | i <- [0 .. arity - 1]]
          let lambda =
                Pre.UEClosure
                  args
                  (Pre.USReturn (Pre.UEApplication (Pre.UEVar x) (map Pre.UEVar args)))
          closeExpression lambda
        Nothing -> pure ([], Post.CEVar x)
  else pure ([], Post.CEVar x)
closeExpression (Pre.UELiteral l) = pure ([], Post.CELiteral l)
closeExpression (Pre.UEList es) = (Post.CEList <$>) . sequence <$> traverse closeExpression es
closeExpression Pre.UESpecial = pure ([], Post.CESpecial)
closeExpression (Pre.UEApplication f args) = do
  res <- readIORef reserved
  funs <- S.fromList . M.keys <$> readIORef functions
  let toplevels = funs <> res

  (p2s, args') <- sequence <$> traverse closeExpression args
  case f of
    Pre.UEVar x | x `S.member` toplevels -> do
      pure (p2s, Post.CEApplication (Post.CEVar x) args')
    _ -> do
      name <- newCallName
      (p1, f') <- closeExpression f
      let callVar = Post.CEVar name
      let callVarP = Post.CEProperty callVar "1"
      let callDict = Post.CEProperty callVar "0"
      let call = Post.CEApplication callVarP (callDict : args')
      pure (p1 <> p2s, Post.CEDeclaration name f' call)
closeExpression (Pre.UEDeclaration name e1 e2) = do
  modifyIORef' locals (S.insert name)
  (p1, e1') <- closeExpression e1
  (p2, e2') <- closeExpression e2
  pure (p1 <> p2, Post.CEDeclaration name e1' e2')
closeExpression (Pre.UEUnMut e) = do
  (stmts, e') <- closeExpression e
  pure (stmts, Post.CEUnMut e')
closeExpression (Pre.UEConditionBranch e1 e2 e3) = do
  (p1, e1') <- closeExpression e1
  (p2, e2') <- closeExpression e2
  (p3, e3') <- closeExpression e3
  pure (p1 <> p2 <> p3, Post.CEConditionBranch e1' e2' e3')
closeExpression (Pre.UEClosure args e) = do
  old <- readIORef locals
  modifyIORef' locals (S.union (S.fromList args))
  (stmts, body) <- closeStatement e
  xs <- first (stmts <>) <$> closeClosure args body
  writeIORef locals old
  pure xs
closeExpression (Pre.UEBlock es) = do
  old <- readIORef locals
  xs <- (Post.CEBlock <$>) . sequence <$> traverse closeStatement es
  writeIORef locals old
  pure xs
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
closeExpression (Pre.UEMutDeclaration name e1 e2) = do
  (p1, e1') <- closeExpression e1
  (p2, e2') <- closeExpression e2
  pure (p1 <> p2, Post.CEMutDeclaration name e1' e2')
closeExpression (Pre.UEMutUpdate name e1 e2) = do
  (p1, e1') <- closeExpression e1
  (p2, e2') <- closeExpression e2
  pure (p1 <> p2, Post.CEMutUpdate (Post.UVariable name) e1' e2')

closePattern
  :: (MonadClosure m) => Pre.UntypedPattern -> m Post.ClosedPattern
closePattern (Pre.UPVariable x) = pure $ Post.CPVariable x
closePattern (Pre.UPLiteral l) = pure $ Post.CPLiteral l
closePattern (Pre.UPConstructor name ps) = do
  ps' <- traverse closePattern ps
  pure $ Post.CPConstructor name ps'
closePattern Pre.UPWildcard = pure Post.CPWildcard
closePattern (Pre.UPSpecialVariable x) = pure $ Post.CPSpecialVar x
closePattern (Pre.UPList ps p) = do
  ps' <- traverse closePattern ps
  p' <- traverse closePattern p
  pure $ Post.CPList ps' p'

closeStatement
  :: (MonadClosure m)
  => Pre.UntypedStatement
  -> m ([Post.ClosedProgram], Post.ClosedStatement)
closeStatement (Pre.USReturn e) = (Post.CSReturn <$>) <$> closeExpression e
closeStatement (Pre.USDeclaration name e1) = do
  modifyIORef' locals (S.insert name)
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
closeStatement (Pre.USMutDeclaration name e) = do
  modifyIORef' locals (S.insert name)
  (stmts, e') <- closeExpression e
  pure (stmts, Post.CSMutDeclaration name e')
closeStatement (Pre.USMutUpdate name e) = do
  (stmts, e') <- closeExpression e
  pure (stmts, Post.CSMutUpdate (Post.UVariable name) e')

makeReturn :: Post.ClosedExpr -> [Post.ClosedStatement]
makeReturn (Post.CEBlock es) = es
makeReturn e = [Post.CSReturn e]

makeReturnBody :: Post.ClosedExpr -> Post.ClosedStatement
makeReturnBody e = Post.CSExpr (Post.CEBlock $ makeReturn e)

closeProgram
  :: (MonadClosure m) => Pre.UntypedProgram -> m [Post.ClosedProgram]
closeProgram (Pre.UPFunction name args e) = do
  lcls <- readIORef locals
  modifyIORef' locals (S.union (S.fromList args))
  modifyIORef' functions (M.insert name (length args))
  (stmts, e') <- closeStatement e
  writeIORef locals lcls
  pure $ stmts ++ [Post.CPFunction name args e']
closeProgram (Pre.UPNativeFunction fp name arity st) = do
  modifyIORef' reserved (S.insert name)
  modifyIORef' functions (M.insert name arity)
  pure [Post.CPNativeFunction fp name arity st]
closeProgram (Pre.UPStatement s) = do
  (stmts, s') <- closeStatement s
  pure $ stmts ++ [Post.CPStatement s']
closeProgram (Pre.UPDeclaration n e) = do
  modifyIORef' globalVars (<> S.singleton n)
  (stmts, e') <- closeExpression e
  pure $ stmts ++ [Post.CPDeclaration n e']
closeProgram (Pre.UPMutDeclaration n e) = do
  modifyIORef' globalVars (<> S.singleton n)
  (stmts, e') <- closeExpression e
  pure $ stmts ++ [Post.CPMutDeclaration n e']
closeProgram (Pre.UPMutUpdate n e) = do
  modifyIORef' globalVars (<> S.singleton n)
  (stmts, e') <- closeExpression e
  pure $ stmts ++ [Post.CPMutUpdate (Post.UVariable n) e']

runClosureConversion
  :: (MonadIO m)
  => [Pre.UntypedProgram]
  -> m (Either Text [Post.ClosedProgram])
runClosureConversion e = do
  writeIORef closedCounter 0
  (concat <$>) <$> runExceptT (traverse closeProgram e)

getDefinedWithoutNatives :: MonadIO m => m (Set Text)
getDefinedWithoutNatives = do
  res <- readIORef reserved
  _lcls <- readIORef locals
  funs <- S.fromList . M.keys <$> readIORef functions
  globals <- readIORef globalVars
  let defined = (funs <> globals) S.\\ res

  pure $ _lcls S.\\ defined