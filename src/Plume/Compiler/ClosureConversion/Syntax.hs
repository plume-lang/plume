module Plume.Compiler.ClosureConversion.Syntax where

import Plume.Syntax.Common.Literal

data ClosedExpr
  = CEVar Text
  | CEApplication ClosedExpr [ClosedExpr]
  | CELiteral Literal
  | CEList [ClosedExpr]
  | CEIndex ClosedExpr ClosedExpr
  | CEAnd ClosedExpr ClosedExpr
  | CEDeclaration Text ClosedExpr ClosedExpr
  | CEConditionBranch ClosedExpr ClosedExpr ClosedExpr
  | CESwitch ClosedExpr [(ClosedPattern, ClosedExpr)]
  | CEDictionary (IntMap ClosedExpr)
  | CEBlock [ClosedStatement]
  | CEProperty ClosedExpr Int
  | CEEqualsType ClosedExpr Text
  | CESpecial
  deriving (Eq, Show, Ord)

data ClosedPattern
  = CPVariable Text
  | CPLiteral Literal
  | CPConstructor Text [ClosedPattern]
  | CPWildcard
  | CPSpecialVar Text
  deriving (Eq, Show, Ord)

data ClosedStatement
  = CSExpr ClosedExpr
  | CSReturn ClosedExpr
  | CSDeclaration Text ClosedExpr
  | CSConditionBranch ClosedExpr ClosedStatement ClosedStatement
  deriving (Eq, Show, Ord)

data ClosedProgram
  = CPFunction Text [Text] ClosedStatement
  | CPStatement ClosedStatement
  | CPNativeFunction Text Text Int
  deriving (Eq, Show, Ord)