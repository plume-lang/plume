module Plume.Compiler.ClosureConversion.Conversion where

import Control.Monad.Except
import Data.Map qualified as M
import Data.Set qualified as S
import GHC.IO
import Plume.Compiler.ClosureConversion.Free
import Plume.Compiler.ClosureConversion.Syntax qualified as Post
import Plume.Compiler.TypeErasure.Syntax qualified as Pre

type MonadClosure m = (MonadIO m, MonadError Text m)

-- | CLOSURE CONVERSION
-- | Closure conversion may be one of the most important steps in the
-- | compilation process. It converts all anonymous functions and closures
-- | into named functions. This is done by creating a new function that
-- | carries an environment (contains all the free variables) and a reference
-- | to the original function.
-- |
-- | - All remaining closures should converted to named functions.
-- |
-- | - When encountering a native function or global variable, it should be
-- |   added to the reserved stack, meaning that we shall not convert this 
-- |   function. However, if the function is used as a variable name in a 
-- |   call, it should be semi-converted.
-- |
-- | - Local stack is used to prevent name conflicts between reserved and locals
-- |   variables. They're defined as a set of text containing the names of the 
-- |   variables encountered in a block scope.
-- |
-- | - Special functions should not be converted, this especially includes 
-- |   ADTs. They're recognized by a special and unique character at the 
-- |   beginning of their name.
-- | 
-- | The conversion process is done by traversing the AST and converting each
-- | expression and statement into a closed expression or statement.
-- | When encountering a closure, the following algorithm is applied:
-- |
-- | - The free variables are calculated, to determine the environment.
-- |
-- | - Reserved functions are eliminated from environment, except for those
-- |   that are used as variables in the closure.
-- |
-- | - A new environment dictionary is created, containing all the free
-- |   variables (e.g. { x: x, y: y }).
-- |
-- | - A new environment variable is created, representing the closure
-- |   environment.
-- |
-- | - The body of the closure is substituted with the environment dictionary
-- |   and the closure environment variable.
-- |
-- | - The closure is converted into a named function, containing the
-- |   environment as the first argument and the rest of the arguments.
-- |
-- | - A new object (resp. dictionary) is created, containing the closure's
-- |   environment and the function reference (e.g. { f: <func>, env: <env> }).

{-# NOINLINE locals #-}
locals :: IORef (S.Set Text)
locals = unsafePerformIO $ newIORef S.empty

{-# NOINLINE reserved #-}
reserved :: IORef (M.Map Text Int)
reserved = unsafePerformIO $ newIORef M.empty

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

closeClosure
  :: (MonadClosure m)
  => [Text]
  -> Post.ClosedStatement
  -> Bool
  -> m ([Post.ClosedProgram], Post.ClosedExpr)
closeClosure args e isAsync = do
  funs <- S.fromList . M.keys <$> readIORef reserved
  lcls <- readIORef locals

  let defined = funs S.\\ lcls

  let env = free e S.\\ (S.fromList args <> defined)

  name <- newLambdaName

  let envVars = M.fromList $ zip (S.toList env) [show x :: Text | x <- [(0 :: Int) ..]]
      envDict =
        Post.CEDictionary . fromList $
          map (\(x, i) -> (i, Post.CEVar x)) (M.toList envVars)
      envL    = M.toList envVars

  let envVar = Post.CEVar $ name <> "_env"
  let envProps = map (second $ Post.CEProperty envVar) envL

  let envDecl =
        map (\(x, i) -> Post.CSDeclaration x (Post.CEProperty envVar i)) envL

  let substBody = substituteMany envProps e

  let body = case substBody of
        Post.CSExpr (Post.CEBlock xs) ->
          Post.CSExpr (Post.CEBlock (envDecl <> xs))
        Post.CSExpr ret -> Post.CSExpr (Post.CEBlock (envDecl <> [Post.CSReturn ret]))
        _ -> case envDecl of
          [] -> substBody
          _ -> Post.CSExpr (Post.CEBlock (envDecl <> [substBody]))

  let lambdaDict =
        Post.CEDictionary $
          fromList [("0", envDict), ("1", Post.CEVar name)]

  return
    ([Post.CPFunction name (name <> "_env" : args) body isAsync], lambdaDict)

tupleMaybe :: (Maybe a, Maybe a) -> Maybe a
tupleMaybe (Just x, _) = Just x
tupleMaybe (_, Just y) = Just y
tupleMaybe _ = Nothing

closeExpression
  :: (MonadClosure m)
  => Pre.UntypedExpr
  -> m ([Post.ClosedProgram], Post.ClosedExpr)
closeExpression (Pre.UEVar x) = do
  reserved' <- readIORef reserved
  locals'   <- readIORef locals

  let newRes = M.keysSet reserved' S.\\ locals'

  if S.member x newRes
    then do
      case M.lookup x reserved' of
        Just arity | arity >= 0 -> do
          let args = [x <> "_arg" <> show i | i <- [0 .. arity - 1]]
          let lambda =
                Pre.UEClosure
                  args
                  (Pre.USReturn (Pre.UEApplication (Pre.UEVar x) (map Pre.UEVar args)))
                  False
          closeExpression lambda
        _ -> pure ([], Post.CEVar x)
    else pure ([], Post.CEVar x)
closeExpression (Pre.UELiteral l) = pure ([], Post.CELiteral l)
closeExpression (Pre.UEList es) = (Post.CEList <$>) . sequence <$> traverse closeExpression es
closeExpression Pre.UESpecial = pure ([], Post.CESpecial)
closeExpression (Pre.UEApplication f args) = do
  reserved' <- readIORef reserved
  locals'   <- readIORef locals

  let newRes = M.keysSet reserved' S.\\ locals'
  
  (stmts1, args') <- mapAndUnzipM closeExpression args

  case f of
    Pre.UEVar x | x `S.member` newRes -> 
      pure (concat stmts1, Post.CEApplication (Post.CEVar x) args')
    
    _ -> do
      name <- newCallName
      (p1, f') <- closeExpression f
      let callVar = Post.CEVar name
      let callVarP = Post.CEProperty callVar "1"
      let callDict = Post.CEProperty callVar "0"
      let call = Post.CEApplication callVarP (callDict : args')
      pure (p1 <> concat stmts1, Post.CEDeclaration name f' call)
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
closeExpression (Pre.UEClosure args e isAsync) = do
  old <- readIORef locals
  modifyIORef' locals (S.union (S.fromList args))
  (stmts, body) <- closeStatement e
  xs <- first (stmts <>) <$> closeClosure args body isAsync
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
closeStatement (Pre.USWhile e s) = do
  (p1, e') <- closeExpression e
  (p2, s') <- closeStatement s
  pure (p1 <> p2, Post.CSWhile e' s')

makeReturn :: Post.ClosedExpr -> [Post.ClosedStatement]
makeReturn (Post.CEBlock es) = es
makeReturn e = [Post.CSReturn e]

makeReturnBody :: Post.ClosedExpr -> Post.ClosedStatement
makeReturnBody e = Post.CSExpr (Post.CEBlock $ makeReturn e)

closeProgram
  :: (MonadClosure m) => Pre.UntypedProgram -> m [Post.ClosedProgram]
closeProgram (Pre.UPADTFunction name args e) = do
  modifyIORef' reserved (M.insert name (length args))
  (stmts, e') <- closeStatement e
  pure $ stmts ++ [Post.CPFunction name args e' False]
closeProgram (Pre.UPDeclare name arity) = do
  modifyIORef' reserved (M.insert name arity)
  pure [Post.CPDeclare name]
closeProgram (Pre.UPFunction name args e isAsync) = do
  modifyIORef' reserved (M.insert name (length args))
  modifyIORef' locals (<> S.fromList args)
  (stmts, e') <- closeStatement e
  pure $ stmts ++ [Post.CPFunction name args e' isAsync]
closeProgram (Pre.UPNativeFunction fp name arity st) = do
  modifyIORef' reserved (M.insert name arity)
  pure [Post.CPNativeFunction fp name arity st]
closeProgram (Pre.UPStatement s) = do
  (stmts, s') <- closeStatement s
  pure $ stmts ++ [Post.CPStatement s']
closeProgram (Pre.UPDeclaration n e) = do
  modifyIORef' reserved (M.insert n (-1))
  (stmts, e') <- closeExpression e
  pure $ stmts ++ [Post.CPDeclaration n e']
closeProgram (Pre.UPMutDeclaration n e) = do
  modifyIORef' reserved (M.insert n (-1))
  (stmts, e') <- closeExpression e
  pure $ stmts ++ [Post.CPMutDeclaration n e']
closeProgram (Pre.UPMutUpdate n e) = do
  modifyIORef' reserved (M.insert n (-1))
  (stmts, e') <- closeExpression e
  pure $ stmts ++ [Post.CPMutUpdate (Post.UVariable n) e']

analyseProgram 
  :: (MonadClosure m) => Pre.UntypedProgram -> m ()
analyseProgram (Pre.UPADTFunction name args _) = do
  modifyIORef' reserved (M.insert name (length args))
analyseProgram (Pre.UPDeclare name arity) = do
  modifyIORef' reserved (M.insert name arity)
analyseProgram (Pre.UPFunction name args _ _) = do
  modifyIORef' reserved (M.insert name (length args))
  modifyIORef' locals (<> S.fromList args)
analyseProgram (Pre.UPNativeFunction _ name arity _) = do
  modifyIORef' reserved (M.insert name arity)
analyseProgram (Pre.UPStatement _) = pure ()
analyseProgram (Pre.UPDeclaration n _) = do
  modifyIORef' reserved (M.insert n (-1))
analyseProgram (Pre.UPMutDeclaration n _) = do
  modifyIORef' reserved (M.insert n (-1))
analyseProgram (Pre.UPMutUpdate n _) = do
  modifyIORef' reserved (M.insert n (-1))
  

runClosureConversion
  :: (MonadIO m)
  => [Pre.UntypedProgram]
  -> m (Either Text [Post.ClosedProgram])
runClosureConversion e = do
  writeIORef closedCounter 0
  (concat <$>) <$> runExceptT (traverse_ analyseProgram e >> traverse closeProgram e)
