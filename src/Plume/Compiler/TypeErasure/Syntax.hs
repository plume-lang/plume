module Plume.Compiler.TypeErasure.Syntax where

import Plume.Syntax.Common.Literal

data UntypedExpr
  = UEVar Text
  | UEApplication UntypedExpr [UntypedExpr]
  | UELiteral Literal
  | UEList [UntypedExpr]
  | UEIndex UntypedExpr UntypedExpr
  | UEDeclaration Text UntypedExpr UntypedExpr
  | UEMutDeclaration Text UntypedExpr UntypedExpr
  | UEMutUpdate Text UntypedExpr UntypedExpr
  | UEConditionBranch UntypedExpr UntypedExpr UntypedExpr
  | UESwitch UntypedExpr [(UntypedPattern, UntypedExpr)]
  | UEAnd UntypedExpr UntypedExpr
  | UEBlock [UntypedStatement]
  | UEEqualsType UntypedExpr Text
  | UEClosure [Text] UntypedStatement
  | UESpecial
  | UEUnMut UntypedExpr
  deriving (Eq, Show, Ord)

data UntypedPattern
  = UPVariable Text
  | UPLiteral Literal
  | UPConstructor Text [UntypedPattern]
  | UPWildcard
  | UPSpecialVariable Text
  | UPList [UntypedPattern] (Maybe UntypedPattern)
  deriving (Eq, Show, Ord)

data UntypedStatement
  = USExpr UntypedExpr
  | USReturn UntypedExpr
  | USDeclaration Text UntypedExpr
  | USConditionBranch UntypedExpr UntypedStatement UntypedStatement
  | USMutDeclaration Text UntypedExpr
  | USMutUpdate Text UntypedExpr
  deriving (Eq, Show, Ord)

data UntypedProgram
  = UPFunction Text [Text] UntypedStatement
  | UPStatement UntypedStatement
  | UPNativeFunction Text Text Int
  | UPDeclaration Text UntypedExpr
  | UPMutDeclaration Text UntypedExpr
  | UPMutUpdate Text UntypedExpr
  deriving (Eq, Show, Ord)