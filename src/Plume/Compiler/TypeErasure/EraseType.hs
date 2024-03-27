{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Plume.Compiler.TypeErasure.EraseType where

import Data.List qualified as List
import Plume.Compiler.TypeErasure.DynamicDispatch.BundleExtensions
import Plume.Compiler.TypeErasure.DynamicDispatch.Dispatch
import Plume.Compiler.TypeErasure.Syntax qualified as Post
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Literal
import Plume.Syntax.Concrete.Expression qualified as Pre (TypeConstructor (..))
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR qualified as Pre

eraseType :: [Pre.TypedExpression PlumeType] -> [Post.UntypedProgram]
eraseType (Pre.EDeclaration (Annotation name _) _ (Pre.EClosure args _ body) Nothing : xs) = do
  let args' = map (\(Annotation n _) -> n) args
  Post.UPFunction name args' (eraseStatement body) : eraseType xs
eraseType (Pre.ENativeFunction n _ (args :->: _) : xs) =
  Post.UPNativeFunction n (length args) : eraseType xs
eraseType (Pre.ELocated e _ : xs) = eraseType (e : xs)
eraseType ext@(Pre.EExtensionDeclaration name _ _ arg _ : _) = do
  let arg' = arg.annotationName
  let (exts, rest, extFuns) = bundleExtensions name ext
  let prog = dispatch (name, arg') exts
  eraseType (extFuns ++ prog : rest)
eraseType (Pre.EType (Annotation tyName _) ts : xs) = do
  map createFunction ts ++ eraseType xs
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
    Just f -> Post.UPFunction n [] (Post.USExpr (f []))
    Nothing -> error "Type constructor not found"
  createFunction (Pre.TConstructor n vars) = case List.lookup n ts' of
    Just f -> Post.UPFunction n args (Post.USReturn (f args))
     where
      args = map createVariant [0 .. length vars - 1]
    Nothing -> error "Type constructor not found"
eraseType (x : xs) = Post.UPStatement (eraseStatement x) : eraseType xs
eraseType [] = []

eraseStatement :: Pre.TypedExpression PlumeType -> Post.UntypedStatement
eraseStatement (Pre.EReturn e) = Post.USReturn (eraseExpr e)
eraseStatement (Pre.EDeclaration (Annotation n _) _ e Nothing) = Post.USDeclaration n (eraseExpr e)
eraseStatement (Pre.EConditionBranch e1 e2 e3) = Post.USConditionBranch (eraseExpr e1) (eraseStatement e2) e3'
 where
  e3' = maybe (Post.USExpr (Post.UEVar "nil")) eraseStatement e3
eraseStatement (Pre.ELocated e _) = eraseStatement e
eraseStatement e = Post.USExpr (eraseExpr e)

eraseExpr :: Pre.TypedExpression PlumeType -> Post.UntypedExpr
eraseExpr (Pre.EVariable x _) = Post.UEVar x
eraseExpr (Pre.EApplication f args) = Post.UEApplication (eraseExpr f) (map eraseExpr args)
eraseExpr (Pre.ELiteral l) = Post.UELiteral l
eraseExpr (Pre.EList es) = Post.UEList (map eraseExpr es)
eraseExpr (Pre.EDeclaration (Annotation n _) _ e1 e2) = case e2 of
  Just e2' -> Post.UEDeclaration n (eraseExpr e1) (eraseExpr e2')
  Nothing -> error "Declaration without a body"
eraseExpr (Pre.EConditionBranch e1 e2 e3) = case e3 of
  Just e3' -> Post.UEConditionBranch (eraseExpr e1) (eraseExpr e2) (eraseExpr e3')
  Nothing -> error "Condition branch without a body"
eraseExpr (Pre.ESwitch e cases) =
  Post.UESwitch
    (eraseExpr e)
    (map (bimap erasePattern eraseExpr) cases)
eraseExpr (Pre.EBlock es) = Post.UEBlock (map eraseStatement es)
eraseExpr (Pre.EClosure args _ body) = Post.UEClosure (map (\(Annotation n _) -> n) args) (eraseStatement body)
eraseExpr (Pre.EExtVariable x _) = Post.UEVar x
eraseExpr (Pre.ELocated e _) = eraseExpr e
eraseExpr (Pre.EEqualsType e t) = Post.UEEqualsType (eraseExpr e) t
eraseExpr (Pre.ENativeFunction {}) = error "Native functions aren't expressions"
eraseExpr (Pre.EExtensionDeclaration {}) = error "Extension declarations aren't expressions"
eraseExpr (Pre.EAnd e1 e2) = Post.UEAnd (eraseExpr e1) (eraseExpr e2)
eraseExpr (Pre.EIndex e i) = Post.UEIndex (eraseExpr e) (eraseExpr i)
eraseExpr (Pre.EReturn _) = error "Return isn't an expression"
eraseExpr (Pre.EType {}) = error "Type isn't an expression"

erasePattern :: Pre.TypedPattern PlumeType -> Post.UntypedPattern
erasePattern (Pre.PVariable x _) = Post.UPVariable x
erasePattern (Pre.PLiteral l) = Post.UPLiteral l
erasePattern (Pre.PConstructor n ps) = Post.UPConstructor n (map erasePattern ps)
erasePattern Pre.PWildcard = Post.UPWildcard
erasePattern (Pre.PSpecialVar x _) = Post.UPSpecialVariable x