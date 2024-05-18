module Plume.TypeChecker.TLIR.Syntax where

import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Literal
import Plume.Syntax.Concrete.Expression (Position, TypeConstructor)
import Plume.TypeChecker.TLIR.Modules.Pattern
import Plume.Syntax.Abstract.Expression (IsStandard)

-- | A more precise AST for the type checker, embedding the type information
-- | in the expressions
data TypedExpression t
  = EVariable Text t
  | EExtVariable Text t t
  | EInstanceVariable Text t
  | ELiteral Literal
  | EList [TypedExpression t]
  | EApplication (TypedExpression t) [TypedExpression t]
  | EEqualsType (TypedExpression t) Text
  | EAnd (TypedExpression t) (TypedExpression t)
  | EIndex (TypedExpression t) (TypedExpression t)
  | EType Text [TypeConstructor t]
  | EInstanceDict Text t [TypedExpression t]
  | EInstanceAccess (TypedExpression t) Int
  | EDeclaration
      (Annotation t)
      (TypedExpression t)
      (Maybe (TypedExpression t))
  | EMutDeclaration
      (Annotation t)
      (TypedExpression t)
      (Maybe (TypedExpression t))
  | EMutUpdate (Annotation t) (TypedExpression t) (Maybe (TypedExpression t))
  | EUnMut (TypedExpression t)
  | EConditionBranch
      (TypedExpression t)
      (TypedExpression t)
      (Maybe (TypedExpression t))
  | EClosure
      [Annotation t]
      t
      (TypedExpression t)
      Position
  | EBlock [TypedExpression t]
  | ESwitch (TypedExpression t) [(TypedPattern t, TypedExpression t)]
  | EReturn (TypedExpression t)
  | ENativeFunction Text Text t IsStandard
  | EEmpty | ESpreadable [TypedExpression t]
  deriving (Show, Eq)
