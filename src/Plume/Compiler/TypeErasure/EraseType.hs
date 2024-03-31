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
  :: IORef ([Post.UntypedProgram], [Post.UntypedProgram], [Post.UntypedProgram])
program = unsafePerformIO $ newIORef ([], [], [])

eraseType :: [Pre.TypedExpression PlumeType] -> IO [Post.UntypedProgram]
eraseType (Pre.EDeclaration (Annotation name _) (Pre.EClosure args _ body) Nothing : xs) = do
  let args' = map (\(Annotation n _) -> n) args
  fun <- Post.UPFunction name args' <$> eraseStatement body
  modifyIORef'
    program
    ( \(natives, exts, stmts) ->
        ( natives
        , exts <> [fun]
        , stmts
        )
    )
  eraseType xs
eraseType (Pre.ENativeFunction fp n (args :->: _) : xs) = do
  modifyIORef'
    program
    ( \(natives, exts, stmts) ->
        ( natives <> [Post.UPNativeFunction fp n (length args)]
        , exts
        , stmts
        )
    )
  eraseType xs
eraseType (Pre.ELocated e _ : xs) = eraseType (e : xs)
eraseType (Pre.EExtensionDeclaration name _ arg (Pre.EClosure args _ b) : xs) = do
  let arg' = arg.annotationName
  let name' = name <> "::" <> createName arg.annotationValue
  modifyIORef
    dispatched
    (Map.insert (arg.annotationValue, name) name')

  fun <-
    Post.UPFunction name' (arg' : map (.annotationName) args) <$> eraseStatement b

  modifyIORef'
    program
    ( \(natives, exts, stmts) ->
        ( natives
        , exts <> [fun]
        , stmts
        )
    )
  eraseType xs
eraseType (Pre.EType tyName ts : xs) = do
  let tys = map createFunction ts
  modifyIORef'
    program
    ( \(natives, exts, stmts) ->
        (natives, exts <> tys, stmts)
    )
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
eraseType (x : xs) = do
  ys <- eraseType xs
  x' <- eraseStatement x
  return (Post.UPStatement x' : ys)
eraseType [] = return []

eraseStatement :: Pre.TypedExpression PlumeType -> IO Post.UntypedStatement
eraseStatement (Pre.EReturn e) = Post.USReturn <$> eraseExpr e
eraseStatement (Pre.EDeclaration (Annotation n _) e Nothing) = Post.USDeclaration n <$> eraseExpr e
eraseStatement (Pre.EConditionBranch e1 e2 e3) = do
  e3' <- maybeM e3 eraseStatement
  case e3' of
    Just e3'' -> Post.USConditionBranch <$> eraseExpr e1 <*> eraseStatement e2 <*> pure e3''
    Nothing -> error "Condition branch without a body"
eraseStatement (Pre.ELocated e _) = eraseStatement e
eraseStatement e = Post.USExpr <$> eraseExpr e

eraseExpr :: Pre.TypedExpression PlumeType -> IO Post.UntypedExpr
eraseExpr (Pre.EVariable x _) = pure $ Post.UEVar x
eraseExpr (Pre.EApplication f args) =
  Post.UEApplication <$> eraseExpr f <*> mapM eraseExpr args
eraseExpr (Pre.ELiteral l) = pure $ Post.UELiteral l
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
eraseExpr (Pre.ESwitch e cases) =
  Post.UESwitch
    <$> eraseExpr e
    <*> mapM (bimapM erasePattern eraseExpr) cases
eraseExpr (Pre.EBlock es) = Post.UEBlock <$> mapM eraseStatement es
eraseExpr (Pre.EClosure args _ body) = Post.UEClosure (map (\(Annotation n _) -> n) args) <$> eraseStatement body
eraseExpr (Pre.EExtVariable x fun t) = do
  case t of
    TypeVar _ -> case fun of
      (t' : _) :->: _ | isNotTVar t' -> Post.UEVar <$> getName t'
      _ -> error "Invalid function type"
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

erase :: [Pre.TypedExpression PlumeType] -> IO [Post.UntypedProgram]
erase xs = do
  writeIORef program ([], [], [])
  xs' <- eraseType xs
  readIORef program >>= \case
    (natives, exts, stmts) -> return (natives <> exts <> stmts <> xs')

findWithKey :: (k -> Bool) -> Map k a -> Maybe (k, a)
findWithKey f = List.find (f . fst) . Map.toList