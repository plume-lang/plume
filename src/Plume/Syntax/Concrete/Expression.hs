{-# LANGUAGE PatternSynonyms #-}

module Plume.Syntax.Concrete.Expression where

-- Expressions are the most basic form of programs.
-- They permit the user to express more complex programs by combining
-- different expressions together. They can be anything from a simple
-- variable to a complex function application.

import Data.Text hiding (map)
import Plume.Syntax.Concrete.Annotation
import Plume.Syntax.Concrete.Literal
import Text.Megaparsec.Pos
import Prelude hiding (intercalate)

type Position = (SourcePos, SourcePos)

data BinaryOperator
  = Plus
  | Minus
  | Times
  | Division
  | Mod
  | Equals
  | NotEquals
  | GreaterThan
  | LesserThan
  | StrictlyGreatherThan
  | StrictlyLesserThan
  | And
  | Or

data PrefixOperator
  = Not

data ConcreteExpression t
  = EVariable Text
  | ELiteral Literal
  | EApplication (ConcreteExpression t) [ConcreteExpression t]
  | EBinary BinaryOperator (ConcreteExpression t) (ConcreteExpression t)
  | EPrefix PrefixOperator (ConcreteExpression t)
  | EDeclaration
      (Annotation (Maybe t))
      (ConcreteExpression t)
      (Maybe (ConcreteExpression t))
  | EConditionBranch
      (ConcreteExpression t)
      (ConcreteExpression t)
      (ConcreteExpression t)
  | EClosure
      [Annotation (Maybe t)]
      (Maybe t)
      (ConcreteExpression t)
  | EBlock [ConcreteExpression t]
  | ERowEmpty
  | ERowExtension Text (ConcreteExpression t) (ConcreteExpression t)
  | ERowSelect (ConcreteExpression t) Text
  | ERowRestrict (ConcreteExpression t) Text
  | ERequire Text
  | ELocated (ConcreteExpression t) Position

pattern (:>:) :: ConcreteExpression t -> Position -> ConcreteExpression t
pattern e :>: p = ELocated e p
