module Plume.Compiler.TypeErasure.Syntax where

import Plume.Syntax.Common.Literal

data UntypedExpr
  = UEVar Text
  | UEApplication UntypedExpr [UntypedExpr]
  | UELiteral Literal
  | UEList [UntypedExpr]
  | UEDeclaration Text UntypedExpr UntypedExpr
  | UEConditionBranch UntypedExpr UntypedExpr UntypedExpr
  | UESwitch UntypedExpr [(UntypedPattern, UntypedExpr)]
  | UEBlock [UntypedStatement]
  | UEProperty UntypedExpr Int
  | UEDictionary (IntMap UntypedExpr)
  | UETypeOf UntypedExpr
  deriving (Eq, Show, Ord)

data UntypedPattern
  = UPVariable Text
  | UPLiteral Literal
  | UPConstructor Text [UntypedPattern]
  | UPWildcard
  deriving (Eq, Show, Ord)

data UntypedStatement
  = USExpr UntypedExpr
  | USReturn UntypedExpr
  | USDeclaration Text UntypedExpr
  | USConditionBranch UntypedExpr UntypedStatement UntypedStatement
  deriving (Eq, Show, Ord)

data UntypedProgram
  = UPFunction Text [Text] UntypedStatement
  | UPStatement UntypedStatement
  | UPNativeFunction Text Int
  deriving (Eq, Show, Ord)