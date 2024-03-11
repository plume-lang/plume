module Plume.Syntax.Concrete (module CST, Expression, Program) where

-- Main core AST module
-- This module re-exports all the other AST modules
-- to be later imported by the parser and the type checker.

import Plume.Syntax.Common.Type
import Plume.Syntax.Concrete.Expression as CST

type Expression = ConcreteExpression PlumeType

type Program = [Expression]
