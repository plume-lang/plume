{-# LANGUAGE TypeFamilies, PatternSynonyms #-}

module Language.Plume.Syntax.HLIR
  ( -- * Re-exports
    module Ann,
    module Lit,
    module Ty,
    module Pos,

    -- * Types
    ModuleId,
    Declaration (..),
    Expression (..),
    Locate(..),
    Pattern (..),

    -- * Patterns for expressions
    pattern MkExprBinary,
    pattern MkExprUnary,

    -- * Type aliases for HLIR and AST
    HLIR,
    AST,
  )
where

import Language.Plume.Syntax.Internal.Annotation as Ann
import Language.Plume.Syntax.Internal.Literal as Lit
import Language.Plume.Syntax.Internal.Type as Ty
import Language.Plume.Syntax.Internal.Position as Pos
import GHC.TypeLits

-- A program is a list of declarations and expressions.

type ModuleId = Text

data Declaration f t
  = MkDeclVariable (Ann.Annotation (f t)) [t] (Expression f t)
  | MkDeclFunction Text [t] [Ann.Annotation (f t)] (f t) (Expression f t)
  | MkDeclRequire ModuleId
  | MkDeclPublic (Declaration f t)
  | MkDeclLocated Pos.Position (Declaration f t)
  | MkDeclNative Text [t] [t] t
  deriving (Eq, Ord, Show)

data Expression f t
  = MkExprLiteral Lit.Literal
  | MkExprVariable (Ann.Annotation (f t))
  | MkExprCall (Expression f t) [Expression f t] (f t)
  | MkExprAnnotation (Expression f t) t
  | MkExprLambda [Ann.Annotation (f t)] (f t) (Expression f t)
  | MkExprIf (Expression f t) (Expression f t) (Expression f t)
  | MkExprLocated Pos.Position (Expression f t)
  | MkExprBlock [Expression f t] (f t)
  | MkExprReturn (Expression f t)
  | MkExprSwitch (Expression f t) [(Pattern f t, Expression f t)] (f t)
  deriving (Eq, Ord, Show)

data Pattern f t
  = MkPatternLiteral Lit.Literal
  | MkPatternVariable (Ann.Annotation (f t))
  | MkPatternWildcard (f t)
  | MkPatternLocated Pos.Position (Pattern f t)
  | MkPatternSpecial (Ann.Annotation (f t)) [Pattern f t]
  deriving (Eq, Ord, Show)

pattern MkExprBinary
  :: Text
  -> Expression Maybe t
  -> Expression Maybe t
  -> Expression Maybe t
pattern MkExprBinary op lhs rhs = MkExprCall (MkExprVariable (MkAnnotation op Nothing)) [lhs, rhs] Nothing

pattern MkExprUnary :: Text -> Expression Maybe t -> Expression Maybe t
pattern MkExprUnary op operand = MkExprCall (MkExprVariable (MkAnnotation op Nothing)) [operand] Nothing

-- HLIR represents the High-Level Intermediate Representation of the Plume
-- language. It is a typed AST that is used for type checking and code
-- generation. It should be fully
type family HLIR (str :: Symbol) where
  HLIR "declaration" = Declaration Identity Ty.PlumeType
  HLIR "expression" = Expression Identity Ty.PlumeType
  HLIR "pattern" = Pattern Identity Ty.PlumeType
  HLIR "type" = Identity Ty.PlumeType

-- AST represents the Abstract Syntax Tree of the Plume language. It is an
-- untyped AST that is used for parsing and semantic analysis.
-- type AST = Declaration Maybe Ty.PlumeType

type family AST (str :: Symbol) where
  AST "declaration" = Declaration Maybe Ty.PlumeType
  AST "expression" = Expression Maybe Ty.PlumeType
  AST "pattern" = Pattern Maybe Ty.PlumeType
  AST "type" = Maybe Ty.PlumeType

instance Locate (Declaration f t) where
  locate = MkDeclLocated

instance Locate (Expression f t) where
  locate = MkExprLocated

instance Locate (Pattern f t) where
  locate = MkPatternLocated