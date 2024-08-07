module Plume.Compiler.ClosureConversion.Syntax where

import Plume.Syntax.Common.Literal
import Plume.Syntax.Abstract (IsStandard)

data ClosedExpr
  = CEVar Text
  | CEApplication ClosedExpr [ClosedExpr]
  | CELiteral Literal
  | CEList [ClosedExpr]
  | CEIndex ClosedExpr ClosedExpr
  | CEAnd ClosedExpr ClosedExpr
  | CEDeclaration Text ClosedExpr ClosedExpr
  | CEMutDeclaration Text ClosedExpr ClosedExpr
  | CEMutUpdate Update ClosedExpr ClosedExpr
  | CEConditionBranch ClosedExpr ClosedExpr ClosedExpr
  | CESwitch ClosedExpr [(ClosedPattern, ClosedExpr)]
  | CEDictionary (Map Text ClosedExpr)
  | CEBlock [ClosedStatement]
  | CEProperty ClosedExpr Text
  | CEEqualsType ClosedExpr Text
  | CESpecial
  | CEUnMut ClosedExpr

  -- Used for desugaring step
  | CEEqualsTo ClosedExpr ClosedExpr
  | CESlice ClosedExpr Int
  deriving (Eq, Show, Ord)

data Update
  = UVariable Text
  | UProperty Update Text
  deriving (Eq, Show, Ord)

data ClosedPattern
  = CPVariable Text
  | CPLiteral Literal
  | CPConstructor Text [ClosedPattern]
  | CPWildcard
  | CPSpecialVar Text
  | CPList [ClosedPattern] (Maybe ClosedPattern)
  deriving (Eq, Show, Ord)

data ClosedStatement
  = CSExpr ClosedExpr
  | CSReturn ClosedExpr
  | CSDeclaration Text ClosedExpr
  | CSConditionBranch ClosedExpr ClosedStatement ClosedStatement
  | CSMutDeclaration Text ClosedExpr
  | CSMutUpdate Update ClosedExpr
  | CSWhile ClosedExpr ClosedStatement
  deriving (Eq, Show, Ord)

data ClosedProgram
  = CPFunction Text [Text] ClosedStatement Bool
  | CPStatement ClosedStatement
  | CPNativeFunction Text Text Int IsStandard
  | CPDeclaration Text ClosedExpr
  | CPMutDeclaration Text ClosedExpr
  | CPMutUpdate Update ClosedExpr
  | CPDeclare Text
  deriving (Eq, Show, Ord)
