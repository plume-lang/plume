module Plume.Compiler.TypeErasure.DynamicDispatch.Dispatch where

import Plume.Compiler.TypeErasure.DynamicDispatch.BundleExtensions
import Plume.Compiler.TypeErasure.DynamicDispatch.RTTI
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Literal
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR
import Control.Monad.Exception (compilerError)

type ExtensionVariable = Text
type ExtensionName = Text
type Extension = (ExtensionName, ExtensionVariable)

dispatch :: Extension -> [Bundled] -> TypedExpression PlumeType
dispatch (extName, extVar) impl = do
  let branches = map (createPatternBranch extVar) impl
  let if' = map createIf branches
  let ifSequence = createIfSequence if'
  EDeclaration
    (Annotation extName TUnit)
    (EClosure [Annotation extVar TUnit] TUnit ifSequence)
    Nothing

createPatternBranch
  :: ExtensionVariable
  -> Bundled
  -> (TypedExpression PlumeType, TypedExpression PlumeType)
createPatternBranch extVar (Bundled name _ ty _) =
  (createConditionAnd $ createCondition (EVariable extVar TUnit) tyName, body)
 where
  tyName = rtti ty
  extName = name <> "::" <> createName ty
  body = EApplication (EVariable extName TUnit) [EVariable extVar TUnit]

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