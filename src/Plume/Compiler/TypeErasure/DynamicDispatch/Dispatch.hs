module Plume.Compiler.TypeErasure.DynamicDispatch.Dispatch where

import Plume.Syntax.Common.Literal
import Plume.TypeChecker.TLIR
import Control.Monad.Exception (compilerError)

type ExtensionVariable = Text
type ExtensionName = Text
type Extension = (ExtensionName, ExtensionVariable)

createIfSequence :: [TypedExpression] -> TypedExpression
createIfSequence [] = EEqualsType (ELiteral (LBool True)) "True"
createIfSequence [EConditionBranch _ body _] = EReturn body
createIfSequence (EConditionBranch cond body _ : xs) = EConditionBranch cond (EReturn body) (Just (createIfSequence xs))
createIfSequence _ = compilerError "Invalid if sequence"

createIf
  :: (TypedExpression, TypedExpression)
  -> TypedExpression
createIf (cond, body) = EConditionBranch cond body Nothing

createConditionAnd :: [TypedExpression] -> TypedExpression
createConditionAnd [] = EEqualsType (ELiteral (LBool True)) "True"
createConditionAnd [x] = x
createConditionAnd (x : xs) = EAnd x (createConditionAnd xs)