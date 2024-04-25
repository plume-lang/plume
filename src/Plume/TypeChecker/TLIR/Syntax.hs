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
  | ELiteral Literal
  | EList [TypedExpression t]
  | EApplication (TypedExpression t) [TypedExpression t]
  | EEqualsType (TypedExpression t) Text
  | EAnd (TypedExpression t) (TypedExpression t)
  | EIndex (TypedExpression t) (TypedExpression t)
  | EType Text [TypeConstructor t]
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
  | EExtensionDeclaration
      Text
      t
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
  | ENativeFunction Text Text t IsStandard
  deriving (Show, Eq)
