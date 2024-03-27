{-# LANGUAGE PatternSynonyms #-}

module Plume.Syntax.Concrete.Expression where

-- Expressions are the most basic form of programs.
-- They permit the user to express more complex programs by combining
-- different expressions together. They can be anything from a simple
-- variable to a complex function application.

import Data.Text hiding (map)
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Literal
import Plume.Syntax.Common.Pattern
import Plume.Syntax.Common.Type
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
  | EList [ConcreteExpression t]
  | EApplication (ConcreteExpression t) [ConcreteExpression t]
  | EBinary BinaryOperator (ConcreteExpression t) (ConcreteExpression t)
  | EPrefix PrefixOperator (ConcreteExpression t)
  | EType (Annotation [PlumeGeneric]) [TypeConstructor t]
  | EDeclaration
      [PlumeGeneric]
      (Annotation (Maybe t))
      (ConcreteExpression t)
      (Maybe (ConcreteExpression t))
  | EConditionBranch
      (ConcreteExpression t)
      (ConcreteExpression t)
      (Maybe (ConcreteExpression t))
  | EClosure
      [Annotation (Maybe t)]
      (Maybe t)
      (ConcreteExpression t)
  | EBlock [ConcreteExpression t]
  | EProperty Text (ConcreteExpression t)
  | ERequire Text
  | ELocated (ConcreteExpression t) Position
  | EMacro Text (ConcreteExpression t)
  | EMacroFunction Text [Text] (ConcreteExpression t)
  | EMacroVariable Text
  | EMacroApplication Text [ConcreteExpression t]
  | ESwitch
      (ConcreteExpression t)
      [(Pattern, ConcreteExpression t)]
  | EReturn (ConcreteExpression t)
  | EGenericProperty [PlumeGeneric] Text [t] t
  | ETypeExtension [PlumeGeneric] (Annotation t) [ExtensionMember t]
  | ENativeFunction Text Text [Text] t

data TypeConstructor t
  = TConstructor Text [t]
  | TVariable Text
  deriving (Eq, Show)

data ExtensionMember t
  = ExtDeclaration
      [PlumeGeneric]
      (Annotation (Maybe t))
      (ConcreteExpression t)

pattern (:>:) :: ConcreteExpression t -> Position -> ConcreteExpression t
pattern e :>: p = ELocated e p
