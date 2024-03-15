module Plume.TypeChecker.TLIR.Syntax where

import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Literal
import Plume.Syntax.Concrete.Expression (Position)
import Plume.TypeChecker.TLIR.Modules.Pattern

data TypedExpression t
  = EVariable Text t
  | ELiteral Literal
  | EApplication (TypedExpression t) [TypedExpression t]
  | EDeclaration
      (Annotation t)
      [Int]
      (TypedExpression t)
      (Maybe (TypedExpression t))
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
  | ENativeFunction Text [Int] t
  deriving (Eq)
