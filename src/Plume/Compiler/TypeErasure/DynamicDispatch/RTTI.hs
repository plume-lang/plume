module Plume.Compiler.TypeErasure.DynamicDispatch.RTTI where

import Plume.Syntax.Common.Literal
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR
import Control.Monad.Exception (compilerError)

data RTTIResult
  = Item RTTIResult [RTTIResult]
  | Single Text
  | Nil

rtti :: PlumeType -> RTTIResult
rtti (TypeApp t xs) = Item (rtti t) (map rtti xs)
rtti (TypeVar _) = Nil
rtti (TypeId n) = Single n
rtti (TypeQuantified _) = Nil

createCondition
  :: TypedExpression -> RTTIResult -> [TypedExpression]
createCondition e (Item (Single "list") [x]) =
  [EEqualsType e "list"] <> createCondition (EIndex e (ELiteral (LInt 0))) x
createCondition e (Single n) = [EEqualsType e n]
createCondition _ Nil = []
createCondition e (Item (Single x) xs) =
  [EEqualsType e x]
    <> concat
      ( zipWith
          (\x' i -> createCondition (EIndex e (ELiteral (LInt i))) x')
          xs
          [0 :: Integer ..]
      )
createCondition _ (Item _ _) = compilerError "Type application equality not supported yet"