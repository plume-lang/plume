module Plume.Compiler.TypeErasure.DynamicDispatch.Dispatch where

import Plume.Syntax.Common.Literal
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR
import Control.Monad.Exception (compilerError)

type ExtensionVariable = Text
type ExtensionName = Text
type Extension = (ExtensionName, ExtensionVariable)

createIfSequence :: [TypedExpression PlumeType] -> TypedExpression PlumeType
createIfSequence [] = EEqualsType (ELiteral (LBool True)) "True"
createIfSequence [EConditionBranch _ body _] = EReturn body
createIfSequence (EConditionBranch cond body _ : xs) = EConditionBranch cond (EReturn body) (Just (createIfSequence xs))
createIfSequence _ = compilerError "Invalid if sequence"

createIf
  :: (TypedExpression PlumeType, TypedExpression PlumeType)
  -> TypedExpression PlumeType
createIf (cond, body) = EConditionBranch cond body Nothing

createConditionAnd :: [TypedExpression PlumeType] -> TypedExpression PlumeType
createConditionAnd [] = EEqualsType (ELiteral (LBool True)) "True"
createConditionAnd [x] = x
createConditionAnd (x : xs) = EAnd x (createConditionAnd xs)