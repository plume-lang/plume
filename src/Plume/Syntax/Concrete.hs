module Plume.Syntax.Concrete (module CST, Expression, Program) where

-- Main core AST module
-- This module re-exports all the other AST modules
-- to be later imported by the parser and the type checker.

import Plume.Syntax.Concrete.Annotation as CST
import Plume.Syntax.Concrete.Expression as CST
import Plume.Syntax.Concrete.Literal as CST
import Plume.Syntax.Concrete.Type as CST

type Expression = ConcreteExpression ConcreteType

type Program = [Expression]
