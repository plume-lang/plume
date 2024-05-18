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
import GHC.Records (HasField(..))

-- | A position is a tuple of two source positions.
-- | The first position is the start position and the second position 
-- | is the end position.
type Position = (SourcePos, SourcePos)

instance HasField "sourceLine" SourcePos Pos where
  hasField r = (\p -> SourcePos (sourceName r) p (sourceColumn r), sourceLine r)

instance HasField "sourceColumn" SourcePos Pos where
  hasField r = (SourcePos (sourceName r) (sourceLine r), sourceColumn r)

instance HasField "sourceName" SourcePos String where
  hasField r = (\p -> SourcePos p (sourceLine r) (sourceColumn r), sourceName r)

-- | A binary operator is an operator that takes two operands.
-- | The operands are placed on the left and right side of the operator.
-- | These operators are used to perform arithmetic and logical operations.
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

-- | A prefix operator is an operator that takes one operand.
-- | The operand is placed on the right side of the operator.
data PrefixOperator
  = Not
  | PrefixSlice
  deriving (Show, Eq)

-- | A postfix operator is an operator that takes one operand.
-- | The operand is placed on the left side of the operator.
data PostfixOperator
  = PostfixSlice
  deriving (Show, Eq)

type IsMutable = Bool

-- | A concrete expression is an expression that is used to represent
-- | a program. It is a more concrete representation of a program than
-- | an abstract syntax tree. It is used to represent the program in a
-- | way that is easier to understand for the user and especially more
-- | natural.
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
  | EInterface {
      interfaceType :: Annotation [t],
      interfaceGenerics :: [PlumeGeneric],
      interfaceMembers :: [Annotation PlumeScheme]
    }
  | EReturn (ConcreteExpression t)
  | ETypeExtension [PlumeGeneric] (Annotation [t]) (Maybe Text) [ExtensionMember t]
  | ENativeFunction Text Text [Text] t
  | ETypeAlias (Annotation [PlumeGeneric]) t
  deriving (Show)

-- | A type constructor is a type that is used to construct a type.
-- | It may be either a type-constructor function or a type-constructor 
-- | variable.
-- | For instance `unit` is a type-constructor variable whereas 
-- | `Ok` is a type-constructor function.
data TypeConstructor t
  = TConstructor Text [t]
  | TVariable Text
  deriving (Show)

-- | An extension member is a member that is used to extend a type.
-- | It is currently only a declaration that may be a function.
data ExtensionMember t
  = ExtDeclaration
      [PlumeGeneric]
      (Annotation (Maybe t))
      (ConcreteExpression t)
  deriving (Show)

-- | Shorthand for a located expression
-- | x :>: p is equivalent to ELocated x p
pattern (:>:) :: ConcreteExpression t -> Position -> ConcreteExpression t
pattern e :>: p = ELocated e p

-- CONCRETE EXPRESSION INSTANCES

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
  EInterface x xs ys == EInterface x' xs' ys' = x == x' && xs == xs' && ys == ys'
  ETypeExtension xs x t ys == ETypeExtension xs' x' t' ys' = xs == xs' && x == x' && ys == ys' && t == t'
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