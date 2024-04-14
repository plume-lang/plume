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
  | BinarySlice
  deriving (Show, Eq)

data PrefixOperator
  = Not
  | PrefixSlice
  deriving (Show, Eq)

data PostfixOperator
  = PostfixSlice
  deriving (Show, Eq)

type IsMutable = Bool

data ConcreteExpression t
  = EVariable Text
  | ELiteral Literal
  | EList [ConcreteExpression t]
  | EApplication (ConcreteExpression t) [ConcreteExpression t]
  | EBinary BinaryOperator (ConcreteExpression t) (ConcreteExpression t)
  | EPrefix PrefixOperator (ConcreteExpression t)
  | EPostfix PostfixOperator (ConcreteExpression t)
  | EType (Annotation [PlumeGeneric]) [TypeConstructor t]
  | EDeclaration
      [PlumeGeneric]
      IsMutable
      (Annotation (Maybe t))
      (ConcreteExpression t)
      (Maybe (ConcreteExpression t))
  | EConditionBranch
      (ConcreteExpression t)
      (ConcreteExpression t)
      (Maybe (ConcreteExpression t))
  | EClosure
      [Annotation (Maybe t, IsMutable)]
      (Maybe t)
      (ConcreteExpression t)
  | EUnMut (ConcreteExpression t)
  | EBlock [ConcreteExpression t]
  | EProperty Text (ConcreteExpression t)
  | EListIndex (ConcreteExpression t) (ConcreteExpression t)
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
  deriving (Show)

data TypeConstructor t
  = TConstructor Text [t]
  | TVariable Text
  deriving (Show)

data ExtensionMember t
  = ExtDeclaration
      [PlumeGeneric]
      (Annotation (Maybe t))
      (ConcreteExpression t)
  deriving (Show)

pattern (:>:) :: ConcreteExpression t -> Position -> ConcreteExpression t
pattern e :>: p = ELocated e p

instance Eq t => Eq (ConcreteExpression t) where
  EVariable x == EVariable y = x == y
  ELiteral x == ELiteral y = x == y
  EList xs == EList ys = xs == ys
  EApplication f xs == EApplication g ys = f == g && xs == ys
  EBinary op x y == EBinary op' x' y' = op == op' && x == x' && y == y'
  EPrefix op x == EPrefix op' x' = op == op' && x == x'
  EPostfix op x == EPostfix op' x' = op == op' && x == x'
  EType _ xs == EType _ ys = xs == ys
  EDeclaration _ m t x y == EDeclaration _ m' t' x' y' =
    m == m' && t == t' && x == x' && y == y'
  EConditionBranch x y z == EConditionBranch x' y' z' = x == x' && y == y' && z == z'
  EClosure xs t x == EClosure xs' t' x' = xs == xs' && t == t' && x == x'
  EUnMut x == EUnMut y = x == y
  EBlock xs == EBlock ys = xs == ys
  EProperty x y == EProperty x' y' = x == x' && y == y'
  EListIndex x y == EListIndex x' y' = x == x' && y == y'
  ERequire x == ERequire y = x == y
  EMacro x y == EMacro x' y' = x == x' && y == y'
  EMacroFunction x xs y == EMacroFunction x' xs' y' = x == x' && xs == xs' && y == y'
  EMacroVariable x == EMacroVariable y = x == y
  EMacroApplication x xs == EMacroApplication x' xs' = x == x' && xs == xs'
  ESwitch x xs == ESwitch x' xs' = x == x' && xs == xs'
  EReturn x == EReturn y = x == y
  EGenericProperty xs x ys y == EGenericProperty xs' x' ys' y' =
    xs == xs' && x == x' && ys == ys' && y == y'
  ETypeExtension xs x ys == ETypeExtension xs' x' ys' = xs == xs' && x == x' && ys == ys'
  ENativeFunction x y xs z == ENativeFunction x' y' xs' z' = x == x' && y == y' && xs == xs' && z == z'
  ELocated x _ == ELocated y _ = x == y
  ELocated x _ == y = x == y
  x == ELocated y _ = x == y
  _ == _ = False

instance Eq t => Eq (TypeConstructor t) where
  TConstructor x xs == TConstructor y ys = x == y && xs == ys
  TVariable x == TVariable y = x == y
  _ == _ = False

instance Eq t => Eq (ExtensionMember t) where
  ExtDeclaration xs x y == ExtDeclaration xs' x' y' = xs == xs' && x == x' && y == y'