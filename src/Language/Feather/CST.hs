module Language.Feather.CST (module CST, Expression, Program) where

-- Main core AST module
-- This module re-exports all the other AST modules
-- to be later imported by the parser and the type checker.

import Language.Feather.CST.Annotation as CST
import Language.Feather.CST.Expression as CST
import Language.Feather.CST.Literal as CST
import Language.Feather.CST.Type as CST

type Expression = ConcreteExpression ConcreteType

type Program = [Expression]
