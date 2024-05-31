{-# LANGUAGE LambdaCase #-}

module Plume.Compiler.TypeErasure.EraseType where

import Data.Bitraversable
import Data.List qualified as List
import GHC.IO hiding (liftIO)
import Plume.Compiler.TypeErasure.Syntax qualified as Post
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Concrete (Position)
import Plume.Syntax.Common.Literal
import Plume.Syntax.Concrete.Expression qualified as Pre (TypeConstructor (..))
import Plume.TypeChecker.Constraints.Solver (isNotTVar)
import Plume.TypeChecker.Constraints.Unification (doesUnifyWith)
import Plume.TypeChecker.Monad
import Plume.TypeChecker.TLIR qualified as Pre
import Control.Monad.Exception (compilerError)

-- | TYPE ERASURE
-- | Type erasure is a simple process that aims at removing all type information
-- | from the program as we don't need it anymore. This is done by converting
-- | all typed expressions to untyped expressions.
-- | 
-- | But, we should take care of some things:
-- |
-- |  - We should transform ADTs in order just to extract functions and 
-- |    declarations.
-- |
-- |  - We should transform extensions in order to remove every Instance
-- |    expression
-- |
-- | Conventionnaly, we transform ADTs with type arguments into functions that
-- | take the same number of arguments as the type arguments and return a
-- | list of untyped expressions. To recognize ADTs, we look for a `special`
-- | expression in the first position of the list. If it's not there, we
-- | consider it as a variable.
-- | 
-- | Special values should not appear by default in the program, they should be
-- | only available from that point.

{-# NOINLINE dispatched #-}
dispatched :: IORef [((PlumeType, Text), Text)]
dispatched = unsafePerformIO $ newIORef mempty

{-# NOINLINE currentPosition #-}
currentPosition :: IORef (Maybe Position)
currentPosition = unsafePerformIO $ newIORef Nothing

{-# NOINLINE program #-}
program
  :: IORef [Post.UntypedProgram]
program = unsafePerformIO $ newIORef []

insertReturnStmt :: Pre.Expression -> Pre.Expression
insertReturnStmt (Pre.EBlock es) = Pre.EBlock es
insertReturnStmt e = Pre.EBlock [Pre.EReturn e]

isNotDecl :: Pre.Expression -> Bool
isNotDecl (Pre.EDeclaration {}) = False
isNotDecl (Pre.EMutDeclaration {}) = False
isNotDecl (Pre.EMutUpdate {}) = False
isNotDecl _ = True

eraseType :: [Pre.TypedExpression PlumeType] -> IO [Post.UntypedProgram]
eraseType (Pre.EDeclaration (Annotation name _) (Pre.EClosure args _ body isA) Nothing : xs) = do
  let args' = map (\(Annotation n _) -> n) args
  let b = insertReturnStmt body
  b' <- eraseStatement b
  let fun = Post.UPFunction name args' b' isA
  modifyIORef' program (<> [fun])
  eraseType xs
eraseType (Pre.EVariableDeclare name arity : xs) = do
  modifyIORef' program (<> [Post.UPDeclare name arity])
  eraseType xs
eraseType (Pre.ENativeFunction fp n (args :->: _) st : xs) = do
  modifyIORef'
    program
    (<> [Post.UPNativeFunction fp n (length args) st])
  eraseType xs
eraseType (Pre.EType tyName ts : xs) = do
  tys <- mapM createFunction ts
  modifyIORef' program (<> tys)
  eraseType xs
 where
  createVariant n = "a" <> show n
  ts' =
    map
      ( \case
          Pre.TVariable n ->
            ( n
            , const $
                Post.UEList
                  [ Post.UESpecial
                  , Post.UELiteral (LString tyName)
                  , Post.UELiteral (LString n)
                  ]
            )
          Pre.TConstructor n _ ->
            ( n
            , \vars ->
                Post.UEList
                  ( [ Post.UESpecial
                    , Post.UELiteral (LString tyName)
                    , Post.UELiteral (LString n)
                    ]
                      <> map Post.UEVar vars
                  )
            )
      )
      ts
  createFunction :: Pre.TypeConstructor PlumeType -> IO Post.UntypedProgram
  createFunction (Pre.TVariable n) = case List.lookup n ts' of
    Just f -> return $ Post.UPDeclaration n (f [])
    Nothing -> compilerError "Type constructor not found"
  createFunction (Pre.TConstructor n vars) = case List.lookup n ts' of
    Just f -> return $ Post.UPADTFunction n args (Post.USReturn (f args))
     where
      args = map createVariant [0 .. length vars - 1]
    Nothing -> compilerError "Type constructor not found"
eraseType (Pre.EDeclaration (Annotation name _) e Nothing : xs) = do
  e' <- eraseExpr e
  modifyIORef' program (<> [Post.UPDeclaration name e'])
  eraseType xs
eraseType (Pre.EMutDeclaration (Annotation name _) e Nothing : xs) = do
  e' <- eraseExpr e
  modifyIORef' program (<> [Post.UPMutDeclaration name e'])
  eraseType xs
eraseType (Pre.EMutUpdate (Annotation name _) e Nothing : xs) = do
  e' <- eraseExpr e
  modifyIORef' program (<> [Post.UPMutUpdate name e'])
  eraseType xs
eraseType (Pre.EEmpty : xs) = eraseType xs
eraseType (x : xs) = do
  x' <- eraseStatement x
  modifyIORef' program (<> [Post.UPStatement x'])
  ys <- eraseType xs
  return (Post.UPStatement x' : ys)
eraseType [] = return []

eraseStatement :: Pre.TypedExpression PlumeType -> IO Post.UntypedStatement
eraseStatement (Pre.EReturn e) =
  transformReturnE . Post.USReturn <$> eraseExpr e
eraseStatement (Pre.EDeclaration (Annotation n _) e Nothing) = Post.USDeclaration n <$> eraseExpr e
eraseStatement (Pre.EMutDeclaration (Annotation n _) e1 Nothing) =
  Post.USMutDeclaration n <$> eraseExpr e1
eraseStatement (Pre.EMutUpdate (Annotation n _) e1 Nothing) =
  Post.USMutUpdate n <$> eraseExpr e1
eraseStatement (Pre.EConditionBranch e1 e2 e3) = do
  e3' <- maybeM e3 eraseStatement
  case e3' of
    Just e3'' -> Post.USConditionBranch <$> eraseExpr e1 <*> eraseStatement e2 <*> pure e3''
    Nothing -> compilerError "Condition branch without a body"
eraseStatement e = Post.USExpr <$> eraseExpr e

eraseExpr :: Pre.TypedExpression PlumeType -> IO Post.UntypedExpr
eraseExpr (Pre.EMutDeclaration (Annotation n _) e1 e2) = do
  e1' <- eraseExpr e1
  e2' <- maybeM e2 eraseExpr
  case e2' of
    Just e2'' -> return $ Post.UEMutDeclaration n e1' e2''
    Nothing -> compilerError "Mut declaration without a body"
eraseExpr (Pre.EMutUpdate (Annotation n _) e1 e2) = do
  e1' <- eraseExpr e1
  e2' <- maybeM e2 eraseExpr
  case e2' of
    Just e2'' -> return $ Post.UEMutUpdate n e1' e2''
    Nothing -> compilerError "Mut update without a body"
eraseExpr (Pre.EVariable x _) = pure $ Post.UEVar x
eraseExpr (Pre.EApplication f args) =
  Post.UEApplication <$> eraseExpr f <*> mapM eraseExpr args
eraseExpr (Pre.ELiteral l) = pure $ Post.UELiteral l
eraseExpr (Pre.EUnMut e) = Post.UEUnMut <$> eraseExpr e
eraseExpr (Pre.EList es) = Post.UEList <$> mapM eraseExpr es
eraseExpr (Pre.EDeclaration (Annotation n _) e1 e2) = case e2 of
  Just e2' -> Post.UEDeclaration n <$> eraseExpr e1 <*> eraseExpr e2'
  Nothing -> compilerError "Declaration without a body"
eraseExpr (Pre.EConditionBranch e1 e2 e3) = case e3 of
  Just e3' ->
    Post.UEConditionBranch
      <$> eraseExpr e1
      <*> eraseExpr e2
      <*> eraseExpr e3'
  Nothing -> compilerError "Condition branch without a body"
eraseExpr (Pre.ESwitch e cases) = do
  e' <- eraseExpr e
  cases' <- mapM (bimapM erasePattern eraseExpr) cases
  return $ Post.UESwitch e' cases'
eraseExpr (Pre.EBlock es) = Post.UEBlock <$> mapM eraseStatement es
eraseExpr (Pre.EClosure args _ body isA) = do
  let b' = insertReturnStmt body
  b <- eraseExpr b'
  return $
    Post.UEClosure
      (map (\(Annotation n _) -> n) args)
      (Post.USExpr b)
      isA
eraseExpr (Pre.EExtVariable x fun t) = do
  let err = compilerError $
          "Invalid function type for extension variable: "
            <> x
            <> " with type"
            <> show fun
  case t of
    TypeVar _ -> case fun of
      (t' : _) :->: _ -> do
        b <- isNotTVar t'
        if b then Post.UEVar <$> getName t' else err
      _ -> err
    _ -> Post.UEVar <$> getName t
 where
  getName :: PlumeType -> IO Text
  getName ty = do
    m <- readIORef dispatched
    findWithKeyM (\(t', n) -> (n == x &&) <$> ty `doesUnifyWith` t') m >>= \case
      Just (_, n) -> return n
      Nothing -> compilerError $ "Extension not found: " <> show x <> " for type " <> show ty
eraseExpr (Pre.EEqualsType e t) = Post.UEEqualsType <$> eraseExpr e <*> pure t
eraseExpr (Pre.ENativeFunction {}) = compilerError "Native functions aren't expressions"
eraseExpr (Pre.EAnd e1 e2) = Post.UEAnd <$> eraseExpr e1 <*> eraseExpr e2
eraseExpr (Pre.EIndex e i) = Post.UEIndex <$> eraseExpr e <*> eraseExpr i
eraseExpr (Pre.EReturn e) = eraseExpr e
eraseExpr (Pre.EType {}) = compilerError "Type isn't an expression"
eraseExpr (Pre.EInstanceVariable name _) = pure $ Post.UEVar name
eraseExpr (Pre.EInstanceAccess expr i) = do
  expr' <- eraseExpr expr
  pure (Post.UEIndex expr' (Post.UELiteral (LInt (toInteger i))))
eraseExpr Pre.EEmpty = error "Should not encounter empty expressions"
eraseExpr (Pre.ESpreadable _) = error "Should not encounter spreadable expressions"
eraseExpr (Pre.EInstanceDict _ _ exprs) = do
  exprs' <- mapM eraseExpr exprs
  return (Post.UEList exprs')
eraseExpr (Pre.EVariableDeclare _ _) = error "Should not encounter variable declarations"

erasePattern :: Pre.TypedPattern PlumeType -> IO Post.UntypedPattern
erasePattern (Pre.PVariable x _) = pure $ Post.UPVariable x
erasePattern (Pre.PLiteral l) = pure $ Post.UPLiteral l
erasePattern (Pre.PConstructor n _ ps) = Post.UPConstructor n <$> mapM erasePattern ps
erasePattern (Pre.PWildcard _) = pure Post.UPWildcard
erasePattern (Pre.PSpecialVar x _) = pure $ Post.UPSpecialVariable x
erasePattern (Pre.PList _ ps t) =
  Post.UPList <$> mapM erasePattern ps <*> maybeM t erasePattern

erase :: [Pre.TypedExpression PlumeType] -> IO [Post.UntypedProgram]
erase xs = do
  writeIORef program []
  void $ eraseType xs
  readIORef program

findWithKey :: (k -> Bool) -> [(k, a)] -> Maybe (k, a)
findWithKey f = List.find (f . fst)

findWithKeyM :: Monad m => (k -> m Bool) -> [(k, a)] -> m (Maybe (k, a))
findWithKeyM f = foldr go (pure Nothing)
 where
  go (k, a) acc = do
    b <- f k
    if b then return $ Just (k, a) else acc

decompose :: [a] -> ([a], Maybe a)
decompose [] = ([], Nothing)
decompose [x] = ([], Just x)
decompose xs = case (viaNonEmpty init xs, viaNonEmpty last xs) of
  (Just init', Just last') -> (init', Just last')
  (_, _) -> ([], Nothing)

transformReturn :: Post.UntypedExpr -> Post.UntypedExpr
transformReturn (Post.UEBlock es) = case decompose es of
  (init', Just (Post.USExpr l)) -> Post.UEBlock (init' <> [transformReturnE (Post.USReturn l)])
  _ -> Post.UEBlock es
transformReturn e = Post.UEBlock [transformReturnE (Post.USReturn e)]

transformReturnE :: Post.UntypedStatement -> Post.UntypedStatement
transformReturnE (Post.USReturn (Post.UESwitch e cases)) = do
  let cases' = map (second transformReturn) cases
  Post.USExpr (Post.UESwitch e cases')
transformReturnE (Post.USReturn e) = Post.USReturn e
transformReturnE e = e
