module Plume.Compiler.TypeErasure.DynamicDispatch.Dispatch where

import Plume.Compiler.TypeErasure.DynamicDispatch.BundleExtensions
import Plume.Compiler.TypeErasure.DynamicDispatch.RTTI
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Literal
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR

type ExtensionVariable = Text
type ExtensionName = Text
type Extension = (ExtensionName, ExtensionVariable)

dispatch :: Extension -> [Bundled] -> TypedExpression PlumeType
dispatch (extName, extVar) impl = do
  let branches = map (createPatternBranch extVar) impl
  let match = ESwitch (ETypeOf (EVariable extVar TUnit)) branches

  EDeclaration
    (Annotation extName TUnit)
    []
    (EClosure [Annotation extVar TUnit] TUnit match)
    Nothing

createPatternBranch
  :: ExtensionVariable
  -> Bundled
  -> (TypedPattern PlumeType, TypedExpression PlumeType)
createPatternBranch extVar (Bundled name _ ty _) =
  (PLiteral (LString tyName), body)
 where
  tyName = rtti ty
  extName = name <> "::" <> tyName
  body = EApplication (EVariable extName TUnit) [EVariable extVar TUnit]
