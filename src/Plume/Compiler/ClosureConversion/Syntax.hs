module Plume.Compiler.ClosureConversion.Syntax where

import Plume.Syntax.Common.Literal
import Plume.TypeChecker.Monad.Type

deriving instance Ord Literal

data ClosedExpr
  = CEVar Text
  | CEApplication ClosedExpr [ClosedExpr]
  | CELiteral Literal
  | CEList [ClosedExpr]
  | CEDeclaration Text ClosedExpr ClosedExpr
  | CEConditionBranch ClosedExpr ClosedStatement ClosedStatement
  | CESwitch ClosedExpr [(ClosedPattern, ClosedStatement)]
  | CEReturn ClosedExpr
  | CENativeFunction Text Int
  | CEDictionary (IntMap ClosedExpr)
  | CEProperty ClosedExpr Int
  deriving (Eq, Show, Ord)

data ClosedPattern
  = CPVariable Text
  | CPLiteral Literal
  | CPConstructor Text [ClosedPattern]
  | CPWildcard
  deriving (Eq, Show, Ord)

data ClosedStatement
  = CSExpr ClosedExpr
  | CSReturn ClosedExpr
  | CSBlock [ClosedStatement]
  | CSDeclaration Text ClosedExpr
  deriving (Eq, Show, Ord)

data ClosedProgram
  = CPFunction Text [Text] ClosedStatement
  | CPExtFunction PlumeType Text [Text] ClosedStatement
  | CPStatement ClosedStatement
  deriving (Eq, Show, Ord)