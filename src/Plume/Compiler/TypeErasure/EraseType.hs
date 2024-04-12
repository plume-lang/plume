{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Plume.Compiler.TypeErasure.EraseType where

import Data.Bitraversable
import Data.List qualified as List
import Data.Map qualified as Map
import GHC.IO
import Plume.Compiler.TypeErasure.DynamicDispatch.BundleExtensions
import Plume.Compiler.TypeErasure.Syntax qualified as Post
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Literal
import Plume.Syntax.Concrete.Expression qualified as Pre (TypeConstructor (..))
import Plume.TypeChecker.Constraints.Solver (isNotTVar)
import Plume.TypeChecker.Constraints.Unification (mgu)
import Plume.TypeChecker.Monad
import Plume.TypeChecker.TLIR qualified as Pre

{-# NOINLINE dispatched #-}
dispatched :: IORef (Map (PlumeType, Text) Text)
dispatched = unsafePerformIO $ newIORef Map.empty

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
eraseType (Pre.EDeclaration (Annotation name _) (Pre.EClosure args _ body) Nothing : xs) = do
  let args' = map (\(Annotation n _) -> n) args
  let b = insertReturnStmt body
  b' <- eraseStatement b
  let fun = Post.UPFunction name args' b'
  modifyIORef' program (<> [fun])
  eraseType xs
eraseType (Pre.ENativeFunction fp n (args :->: _) : xs) = do
  modifyIORef'
    program
    (<> [Post.UPNativeFunction fp n (length args)])
  eraseType xs
eraseType (Pre.ELocated e _ : xs) = eraseType (e : xs)
eraseType (Pre.EExtensionDeclaration name _ arg (Pre.EClosure args _ b) : xs) = do
  let arg' = arg.annotationName
  let name' = name <> "::" <> createName arg.annotationValue
  modifyIORef
    dispatched
    (Map.insert (arg.annotationValue, name) name')
  let b' = insertReturnStmt b
  fun <-
    Post.UPFunction name' (arg' : map (.annotationName) args) <$> eraseStatement b'

  modifyIORef' program (<> [fun])
  eraseType xs
eraseType (Pre.EType tyName ts : xs) = do
  let tys = map createFunction ts
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
  createFunction :: Pre.TypeConstructor PlumeType -> Post.UntypedProgram
  createFunction (Pre.TVariable n) = case List.lookup n ts' of
    Just f -> Post.UPStatement (Post.USDeclaration n (f []))
    Nothing -> error "Type constructor not found"
  createFunction (Pre.TConstructor n vars) = case List.lookup n ts' of
    Just f -> Post.UPFunction n args (Post.USReturn (f args))
     where
      args = map createVariant [0 .. length vars - 1]
    Nothing -> error "Type constructor not found"
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
    Nothing -> error "Condition branch without a body"
eraseStatement (Pre.ELocated e _) = eraseStatement e
eraseStatement e = Post.USExpr <$> eraseExpr e

eraseExpr :: Pre.TypedExpression PlumeType -> IO Post.UntypedExpr
eraseExpr (Pre.EMutDeclaration (Annotation n _) e1 e2) = do
  e1' <- eraseExpr e1
  e2' <- maybeM e2 eraseExpr
  case e2' of
    Just e2'' -> return $ Post.UEMutDeclaration n e1' e2''
    Nothing -> error "Mut declaration without a body"
eraseExpr (Pre.EMutUpdate (Annotation n _) e1 e2) = do
  e1' <- eraseExpr e1
  e2' <- maybeM e2 eraseExpr
  case e2' of
    Just e2'' -> return $ Post.UEMutUpdate n e1' e2''
    Nothing -> error "Mut update without a body"
eraseExpr (Pre.EVariable x _) = pure $ Post.UEVar x
eraseExpr (Pre.EApplication f args) =
  Post.UEApplication <$> eraseExpr f <*> mapM eraseExpr args
eraseExpr (Pre.ELiteral l) = pure $ Post.UELiteral l
eraseExpr (Pre.EUnMut e) = Post.UEUnMut <$> eraseExpr e
eraseExpr (Pre.EList es) = Post.UEList <$> mapM eraseExpr es
eraseExpr (Pre.EDeclaration (Annotation n _) e1 e2) = case e2 of
  Just e2' -> Post.UEDeclaration n <$> eraseExpr e1 <*> eraseExpr e2'
  Nothing -> error "Declaration without a body"
eraseExpr (Pre.EConditionBranch e1 e2 e3) = case e3 of
  Just e3' ->
    Post.UEConditionBranch
      <$> eraseExpr e1
      <*> eraseExpr e2
      <*> eraseExpr e3'
  Nothing -> error "Condition branch without a body"
eraseExpr (Pre.ESwitch e cases) = do
  e' <- eraseExpr e
  cases' <- mapM (bimapM erasePattern eraseExpr) cases
  return $ Post.UESwitch e' cases'
eraseExpr (Pre.EBlock es) = Post.UEBlock <$> mapM eraseStatement es
eraseExpr (Pre.EClosure args _ body) = do
  let b' = insertReturnStmt body
  b <- eraseExpr b'
  return $
    Post.UEClosure
      (map (\(Annotation n _) -> n) args)
      (Post.USExpr b)
eraseExpr (Pre.EExtVariable x fun t) = do
  case t of
    TypeVar _ -> case fun of
      (t' : _) :->: _ | isNotTVar t' -> Post.UEVar <$> getName t'
      _ ->
        error $
          "Invalid function type for extension variable: "
            <> x
            <> " with type"
            <> show fun
    _ -> Post.UEVar <$> getName t
 where
  getName :: PlumeType -> IO Text
  getName ty = do
    m <- readIORef dispatched
    case findWithKey (\(t', n) -> n == x && isRight (mgu ty t')) m of
      Just (_, n) -> return n
      Nothing -> error $ "Extension not found: " <> show x <> " for type " <> show ty
eraseExpr (Pre.ELocated e _) = eraseExpr e
eraseExpr (Pre.EEqualsType e t) = Post.UEEqualsType <$> eraseExpr e <*> pure t
eraseExpr (Pre.ENativeFunction {}) = error "Native functions aren't expressions"
eraseExpr (Pre.EExtensionDeclaration {}) = error "Extension declarations aren't expressions"
eraseExpr (Pre.EAnd e1 e2) = Post.UEAnd <$> eraseExpr e1 <*> eraseExpr e2
eraseExpr (Pre.EIndex e i) = Post.UEIndex <$> eraseExpr e <*> eraseExpr i
eraseExpr (Pre.EReturn _) = error "Return isn't an expression"
eraseExpr (Pre.EType {}) = error "Type isn't an expression"

erasePattern :: Pre.TypedPattern PlumeType -> IO Post.UntypedPattern
erasePattern (Pre.PVariable x _) = pure $ Post.UPVariable x
erasePattern (Pre.PLiteral l) = pure $ Post.UPLiteral l
erasePattern (Pre.PConstructor n ps) = Post.UPConstructor n <$> mapM erasePattern ps
erasePattern Pre.PWildcard = pure Post.UPWildcard
erasePattern (Pre.PSpecialVar x _) = pure $ Post.UPSpecialVariable x
erasePattern (Pre.PList ps t) =
  Post.UPList <$> mapM erasePattern ps <*> maybeM t erasePattern

erase :: [Pre.TypedExpression PlumeType] -> IO [Post.UntypedProgram]
erase xs = do
  writeIORef program []
  void $ eraseType xs
  readIORef program

findWithKey :: (k -> Bool) -> Map k a -> Maybe (k, a)
findWithKey f = List.find (f . fst) . Map.toList

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
