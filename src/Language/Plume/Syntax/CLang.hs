{-# LANGUAGE TypeFamilies #-}

module Language.Plume.Syntax.CLang (
  module Ann,
  module Lit,
  module Ty,

  -- * Types
  Declaration (..),
  Expression (..),

  -- * Type aliases for HLIR and AST
  CLang,
) where

import GHC.Show qualified as S
import Language.Plume.Syntax.Internal.Annotation as Ann
import Language.Plume.Syntax.Internal.Literal as Lit
import Language.Plume.Syntax.Internal.Type as Ty
import GHC.TypeLits

type CLangType = Text

data Declaration
  = MkDeclFunction Text [Ann.Annotation CLangType] CLangType [Expression]
  | MkDeclExtern Text [CLangType] CLangType
  | MkDeclStruct Text [Ann.Annotation CLangType]
  | MkDeclTypedef Text [CLangType] CLangType
  deriving (Eq, Ord)


data Expression
  = MkExprLiteral Lit.Literal
  | MkExprVariable Text
  | MkExprLet Text CLangType Expression
  | MkExprCall Expression [Expression]
  | MkExprIf Expression [Expression] [Expression]
  | MKExprTernary Expression Expression Expression
  | MkExprField Expression Text
  | MkExprStruct Text [Ann.Annotation Expression]
  | MkExprCast Expression CLangType
  | MkExprReturn Expression
  | MkExprDeref Expression
  | MkExprRef Expression
  | MkExprBinary Expression Text Expression
  | MkExprVarDeclare Text CLangType
  | MkExprVarUpdate Text Expression
  deriving (Eq, Ord)

type family CLang (str :: Symbol) where
  CLang "declaration" = Declaration
  CLang "expression" = Expression
  CLang "type" = CLangType

instance Show Declaration where
  show (MkDeclFunction name args ret exprs) = 
    toString ret 
      <> " "
      <> toString name
      <> "("
      <> intercalate ", " (map showAnnot args)
      <> ") {\n"
      <> intercalate ";\n" (map show exprs)
      <> ";\n};"
  show (MkDeclExtern name args ret) =
    "extern "
      <> toString ret
      <> " "
      <> toString name
      <> "("
      <> intercalate ", " (map toString args)
      <> ");"
  show (MkDeclStruct name fields) =
    "struct "
      <> toString name
      <> " {\n"
      <> concatMap ((<> ";") . showAnnot) fields
      <> "\n};"
  show (MkDeclTypedef name args ret) =
    "typedef "
      <> toString ret
      <> " (*"
      <> toString name
      <> ")("
      <> intercalate ", " (map toString args)
      <> ");"

showAnnot :: Ann.Annotation CLangType -> String
showAnnot (Ann.MkAnnotation name ty) = toString ty <> " " <> toString name

instance Show Expression where
  show (MkExprLiteral (MkBool b)) = if b then "1" else "0"
  show (MkExprLiteral lit) = show lit
  show (MkExprVariable name) = toString name
  show (MkExprLet name ty expr) = toString ty <> " " <> toString name <> " = " <> show expr <> ";"
  show (MkExprCall expr args) = show expr <> "(" <> intercalate ", " (map show args) <> ")"
  show (MkExprIf cond t f) = "if (" <> show cond <> ") " <> "{ " <> intercalate "; " (map show t) <> " } else { " <> intercalate "; " (map show f) <> " }"
  show (MKExprTernary cond t f) = show cond <> " ? " <> show t <> " : " <> show f
  show (MkExprField expr name) = show expr <> "." <> toString name
  show (MkExprStruct name fields) = "(struct " <> toString name <> ") { " <> intercalate ", " (map showAnnotE fields) <> " }"
  show (MkExprCast expr ty) = "(" <> toString ty <> ") " <> show expr
  show (MkExprReturn expr) = "return " <> show expr <> ";"
  show (MkExprDeref expr) = "*" <> show expr
  show (MkExprRef expr) = "&" <> show expr
  show (MkExprBinary l op r) = show l <> " " <> toString op <> " " <> show r
  show (MkExprVarDeclare name ty) = toString ty <> " " <> toString name <> ";"
  show (MkExprVarUpdate name expr) = toString name <> " = " <> show expr <> ";"

showAnnotE :: Ann.Annotation Expression -> String
showAnnotE (Ann.MkAnnotation name e) = "." <> toString name <> " = " <> show e