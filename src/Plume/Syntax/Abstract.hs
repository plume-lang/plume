module Plume.Syntax.Abstract (module AST, Expression, Program) where

-- Main core AST module
-- This module re-exports all the other AST modules
-- to be later imported by the parser and the type checker.

import Plume.Syntax.Abstract.Expression as AST
import Plume.Syntax.Concrete.Annotation as AST
import Plume.Syntax.Concrete.Type as AST

type Expression = AbstractExpression ConcreteType

type Program = [Expression]
