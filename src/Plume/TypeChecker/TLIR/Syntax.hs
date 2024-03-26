module Plume.TypeChecker.TLIR.Syntax where

import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Literal
import Plume.Syntax.Concrete.Expression (Position, TypeConstructor)
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR.Modules.Pattern

data TypedExpression t
  = EVariable Text t
  | EExtVariable Text t
  | ELiteral Literal
  | EList [TypedExpression t]
  | EApplication (TypedExpression t) [TypedExpression t]
  | EEqualsType (TypedExpression t) Text
  | EAnd (TypedExpression t) (TypedExpression t)
  | EIndex (TypedExpression t) (TypedExpression t)
  | EType (Annotation [PlumeGeneric]) [TypeConstructor t]
  | EDeclaration
      (Annotation t)
      [PlumeGeneric]
      (TypedExpression t)
      (Maybe (TypedExpression t))
  | EExtensionDeclaration
      (Annotation t)
      t
      [PlumeGeneric]
      (Annotation t)
      (TypedExpression t)
  | EConditionBranch
      (TypedExpression t)
      (TypedExpression t)
      (Maybe (TypedExpression t))
  | EClosure
      [Annotation t]
      t
      (TypedExpression t)
  | EBlock [TypedExpression t]
  | ELocated (TypedExpression t) Position
  | ESwitch (TypedExpression t) [(TypedPattern t, TypedExpression t)]
  | EReturn (TypedExpression t)
  | ENativeFunction Text [PlumeGeneric] t
  deriving (Eq, Show)
