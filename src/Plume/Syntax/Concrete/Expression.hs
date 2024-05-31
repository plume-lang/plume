{-# LANGUAGE PatternSynonyms #-}

module Plume.Syntax.Concrete.Expression where

-- Expressions are the most basic form of programs.
-- They permit the user to express more complex programs by combining
-- different expressions together. They can be anything from a simple
-- variable to a complex function application.

import Data.Text hiding (map)
import GHC.Records (HasField (..))
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Literal
import Plume.Syntax.Common.Pattern
import Plume.Syntax.Common.Type
import Text.Megaparsec.Pos
import Prelude hiding (intercalate)

type IsStandard = Bool

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

type LibraryType = Text

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

-- | A concrete expression is an expression that is used to represent
-- | a program. It is a more concrete representation of a program than
-- | an abstract syntax tree. It is used to represent the program in a
-- | way that is easier to understand for the user and especially more
-- | natural.
data Expression t f 
  = EVariable Identifier (f t)
  | ELiteral Literal
  | EList [Expression t f]
  | EApplication (Expression t f) [Expression t f]
  | EType (Annotation [Text]) [TypeConstructor t]
  | EDeclaration [PlumeGeneric] (Annotation (f t)) (Expression t f) (Maybe (Expression t f))
  | EMutUpdate (Annotation (f t)) (Expression t f) (Maybe (Expression t f))
  | EConditionBranch (Expression t f) (Expression t f) (Maybe (Expression t f))
  | EClosure [Annotation (f t)] (f t) (Expression t f)
  | EBlock [Expression t f]
  | ERequire Text
  | ELocated (Expression t f) Position
  | ESwitch (Expression t f) [(Pattern t f, Expression t f)]
  | EInterface { interfaceType :: Annotation [t], interfaceGenerics :: [PlumeGeneric], interfaceMembers :: [Annotation PlumeScheme] }
  | EReturn (Expression t f)
  | ETypeExtension [PlumeGeneric] (Annotation [t]) (Maybe Text) [ExtensionMember t f]
  | ENativeFunction Text Text [Text] t LibraryType IsStandard
  | ETypeAlias (Annotation [Text]) t
  | EVariableDeclare [PlumeGeneric] Text (f t)
  | EAwait (Expression t f)
  | EInstanceVariable Identifier (f t)
  | EInstanceAccess (Expression t f) Int
  | EInstanceDict Text (f t) [Expression t f]

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
data ExtensionMember t f
  = ExtDeclaration
      [PlumeGeneric]
      (Annotation (f t))
      (Expression t f)

deriving instance (Show t, Show (f t)) => Show (ExtensionMember t f)
deriving instance (Show t, Show (f t)) => Show (Expression t f)

-- | Shorthand for a located expression
-- | x :>: p is equivalent to ELocated x p
pattern (:>:) :: Expression t f -> Position -> Expression t f
pattern e :>: p = ELocated e p

-- CONCRETE EXPRESSION INSTANCES

instance (Eq t, Eq (f t)) => Eq (Expression t f) where
  EVariable x t == EVariable y t' = x == y && t == t'
  ELiteral x == ELiteral y = x == y
  EList xs == EList ys = xs == ys
  EApplication f xs == EApplication g ys = f == g && xs == ys
  EType _ xs == EType _ ys = xs == ys
  EDeclaration _ t x y == EDeclaration _ t' x' y' =
    t == t' && x == x' && y == y'
  EConditionBranch x y z == EConditionBranch x' y' z' = x == x' && y == y' && z == z'
  EClosure xs t x == EClosure xs' t' x' = xs == xs' && t == t' && x == x'
  EBlock xs == EBlock ys = xs == ys
  ERequire x == ERequire y = x == y
  ESwitch x xs == ESwitch x' xs' = x == x' && xs == xs'
  EReturn x == EReturn y = x == y
  EInterface x xs ys == EInterface x' xs' ys' = x == x' && xs == xs' && ys == ys'
  ETypeExtension xs x t ys == ETypeExtension xs' x' t' ys' = xs == xs' && x == x' && ys == ys' && t == t'
  ENativeFunction x y xs z t isStd == ENativeFunction x' y' xs' z' t' isStd' = x == x' && y == y' && xs == xs' && z == z' && t == t' && isStd == isStd'
  EVariableDeclare xs x t == EVariableDeclare xs' x' t' = xs == xs' && x == x' && t == t'
  ELocated x _ == ELocated y _ = x == y
  ELocated x _ == y = x == y
  x == ELocated y _ = x == y
  _ == _ = False

instance (Eq t) => Eq (TypeConstructor t) where
  TConstructor x xs == TConstructor y ys = x == y && xs == ys
  TVariable x == TVariable y = x == y
  _ == _ = False

instance (Eq t, Eq (f t)) => Eq (ExtensionMember t f) where
  ExtDeclaration xs x y == ExtDeclaration xs' x' y' = xs == xs' && x == x' && y == y'