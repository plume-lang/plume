module Plume.Compiler.TypeErasure.DynamicDispatch.Dispatch where

import Plume.Compiler.ClosureConversion.Syntax
import Plume.Compiler.TypeErasure.DynamicDispatch.BundleExtensions
import Plume.Compiler.TypeErasure.DynamicDispatch.RTTI
import Plume.Syntax.Common.Literal

type ExtensionVariable = Text
type ExtensionName = Text
type Extension = (ExtensionName, ExtensionVariable)

dispatch :: Extension -> [Bundled] -> ClosedProgram
dispatch (extName, extVar) impl = do
  let branches = map (createPatternBranch extVar) impl
  let match = CESwitch (CETypeOf (CEVar extVar)) branches

  CPFunction extName [extVar] (CSExpr match)

createPatternBranch
  :: ExtensionVariable -> Bundled -> (ClosedPattern, ClosedExpr)
createPatternBranch extVar (Bundled name _ ty _) =
  (CPLiteral (LString tyName), body)
 where
  tyName = rtti ty
  extName = name <> "::" <> tyName
  body = CEApplication (CEVar extName) [CEVar extVar]
