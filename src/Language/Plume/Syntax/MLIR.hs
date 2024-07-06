{-# LANGUAGE TypeFamilies #-}

module Language.Plume.Syntax.MLIR (
  module Ann,
  module Lit,
  module Ty,
  module Pos,

  -- * Types
  Declaration (..),
  Expression (..),

  -- * Type aliases for HLIR and AST
  MLIR,
) where

import Language.Plume.Syntax.Internal.Annotation as Ann
import Language.Plume.Syntax.Internal.Literal as Lit
import Language.Plume.Syntax.Internal.Type as Ty
import Language.Plume.Syntax.Internal.Position as Pos
import GHC.TypeLits
import GHC.Show qualified as S

data Declaration t
  = MkDeclFunction Text [t] [Ann.Annotation t] t (Expression t)
  | MkDeclVariable (Ann.Annotation t) [t] (Expression t)
  | MkDeclNative Text [t] [t] t
  | MkDeclExtend [t] Text [Ann.Annotation t] t (Expression t)
  deriving (Eq, Ord, Show)

data Expression t
  = MkExprLiteral Lit.Literal
  | MkExprVariable Text t
  | MkExprLet Text t (Expression t) t (Maybe (Expression t))
  | MkExprCall (Expression t) [Expression t] t
  | MkExprIf (Expression t) (Expression t) (Expression t)
  | MkExprBlock [Expression t] t
  | MkExprField (Expression t) Text t
  | MkExprTupleAccess (Expression t) Int t
  | MkExprLambda [Ann.Annotation t] t (Expression t)
  | MkExprPack [Ann.Annotation t] (Expression t, [Ann.Annotation (Expression t)]) t
  | MkExprClosureCall (Expression t) [Expression t] t
  | MkExprReturn (Expression t)
  | MkExprLocated Pos.Position (Expression t)
  deriving (Eq, Ord)

type family MLIR (str :: Symbol) where
  MLIR "declaration" = Declaration Ty.PlumeType
  MLIR "expression" = Expression Ty.PlumeType
  MLIR "type" = Ty.PlumeType

instance Show t => Show (Expression t) where
  show (MkExprLiteral l) = show l
  show (MkExprVariable n _) = toString n
  show (MkExprLet n t e _ Nothing) = "let " <> toString n <> " : " <> S.show t <> " = " <> S.show e
  show (MkExprLet n t e _ (Just b)) = "let " <> toString n <> " : " <> S.show t <> " = " <> S.show e <> " in " <> S.show b
  show (MkExprCall e args _) = S.show e <> "(" <> foldr (\a b -> S.show a <> ", " <> b) "" args <> ")"
  show (MkExprIf c t e) = "if " <> S.show c <> " then " <> S.show t <> " else " <> S.show e
  show (MkExprBlock es _) = "{ \n" <> foldr (\a b -> "  " <> S.show a <> ";\n" <> b) "" es <> " \n}"
  show (MkExprField e f _) = S.show e <> "." <> toString f
  show (MkExprTupleAccess e i _) = S.show e <> "." <> show i
  show (MkExprLambda args _ e) = "fn " <> intercalate ", " (map show args) <> " => " <> S.show e
  show (MkExprPack anns (f, env) _) = "pack " <> S.show f <> " with " <> foldr (\a b -> S.show a <> ", " <> b) "" anns <> " in " <> S.show env
  show (MkExprClosureCall e args _) = S.show e <> "(" <> foldr (\a b -> S.show a <> ", " <> b) "" args <> ")"
  show (MkExprReturn e) = "return " <> S.show e
  show (MkExprLocated _ e) = S.show e
