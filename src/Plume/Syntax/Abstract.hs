module Plume.Syntax.Abstract (module AST, Expression, Program) where

-- Main core AST module
-- This module re-exports all the other AST modules
-- to be later imported by the parser and the type checker.

import Plume.Syntax.Abstract.Expression as AST
import Plume.Syntax.Common.Type

type Expression = AbstractExpression PlumeType

type Program = [Expression]
