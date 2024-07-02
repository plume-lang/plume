{-# LANGUAGE TypeFamilies #-}

module Language.Plume.Syntax.LLIR (
  module Ann,
  module Lit,
  module Ty,

  -- * Types
  Declaration (..),
  Expression (..),

  -- * Type aliases for HLIR and AST
  LLIR,
) where

import Language.Plume.Syntax.Internal.Annotation as Ann
import Language.Plume.Syntax.Internal.Literal as Lit
import Language.Plume.Syntax.Internal.Type as Ty
import GHC.TypeLits

data Declaration t
  = MkDeclFunction Text [t] [Ann.Annotation t] t (Expression t)
  | MkDeclNative Text [t] [t] t
  | MkDeclStruct Text [Ann.Annotation t]
  deriving (Eq, Ord, Show)

data Expression t
  = MkExprLiteral Lit.Literal
  | MkExprVariable Text t
  | MkExprLet Text t (Expression t) (Maybe (Expression t))
  | MkExprCall (Expression t) [Expression t] t
  | MkExprIf (Expression t) (Expression t) (Expression t) t
  | MkExprTernary (Expression t) (Expression t) (Expression t) t
  | MkExprBlock [Expression t]
  | MkExprField (Expression t) Text t t
  | MkExprStruct Text [Ann.Annotation (Expression t)]
  | MkExprCast (Expression t) t
  | MkExprReturn (Expression t)
  | MkExprPtrField (Expression t) Text t
  | MkExprRef (Expression t) t
  deriving (Eq, Ord, Show)

type family LLIR (str :: Symbol) where
  LLIR "declaration" = Declaration Ty.PlumeType
  LLIR "expression" = Expression Ty.PlumeType
  LLIR "type" = Ty.PlumeType
