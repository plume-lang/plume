{-# LANGUAGE PatternSynonyms #-}
module Plume.TypeChecker.TLIR (
  Expression,
  TypedExpression,
  Pattern,
  Program,

  module AST,
  module CST,

  pattern EEqualsType,
  pattern EAnd,
  pattern EIndex,
  pattern ESpreadable,
  pattern EUnMut,

  containsReturn,
  isBlock
) where

import Plume.Syntax.Common.Pattern qualified as CST
import Plume.Syntax.Concrete.Expression qualified as CST
import Plume.Syntax.Concrete as AST hiding (Expression, Pattern, Program, pattern EUnMut)
import Plume.TypeChecker.Monad.Type
import Plume.Syntax.Common.Literal

type Expression = CST.Expression PlumeType Identity
type TypedExpression = Expression
type Pattern = CST.Pattern PlumeType Identity
type Program = [Expression]

pattern EEqualsType :: TypedExpression -> Text -> TypedExpression
pattern EEqualsType e t = EApplication (EVariable "#equalsType" (Identity TUnit)) [e, ELiteral (LString t)]

pattern EAnd :: TypedExpression -> TypedExpression -> TypedExpression
pattern EAnd x y = EApplication (EVariable "#and" (Identity TUnit)) [x, y]

pattern EIndex :: TypedExpression -> TypedExpression -> TypedExpression
pattern EIndex e i = EApplication (EVariable "#index" (Identity TUnit)) [e, i]

pattern ESpreadable :: [TypedExpression] -> TypedExpression
pattern ESpreadable es = EApplication (EVariable "#spreadable" (Identity TUnit)) es

pattern EUnMut :: TypedExpression -> TypedExpression
pattern EUnMut e = EApplication (EVariable "#deref" (Identity TUnit)) [e]

containsReturn :: Expression -> Bool
containsReturn (EBlock es) = any containsReturn es
containsReturn (EReturn _) = True
containsReturn (EConditionBranch _ t f) = containsReturn t || containsReturn f
containsReturn (ESwitch _ cases) = any (containsReturn . snd) cases
containsReturn _ = False

isBlock :: Expression -> Bool
isBlock EBlock{} = True
isBlock _ = False