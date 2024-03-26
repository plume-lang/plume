module Plume.Compiler.TypeErasure.DynamicDispatch.RTTI where

import Plume.Syntax.Common.Literal
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR

data RTTIResult
  = Item RTTIResult [RTTIResult]
  | Single Text
  | Nil

rtti :: PlumeType -> RTTIResult
rtti (TApp t xs) = Item (rtti t) (map rtti xs)
rtti (TVar _) = Nil
rtti (TId n) = Single n

createCondition
  :: TypedExpression PlumeType -> RTTIResult -> [TypedExpression PlumeType]
createCondition e (Item (Single "[]") [x]) =
  [EEqualsType e "[]"] <> createCondition (EIndex e (ELiteral (LInt 0))) x
createCondition e (Single n) = [EEqualsType e n]
createCondition _ Nil = []
createCondition _ (Item _ _) = error "Type application equality not supported yet"