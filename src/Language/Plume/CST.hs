module Language.Plume.CST (module CST, Expression, Program) where

-- Main core AST module
-- This module re-exports all the other AST modules
-- to be later imported by the parser and the type checker.

import Language.Plume.CST.Annotation as CST
import Language.Plume.CST.Expression as CST
import Language.Plume.CST.Literal as CST
import Language.Plume.CST.Type as CST

type Expression = ConcreteExpression ConcreteType

type Program = [Expression]
