module Plume.Syntax.Abstract (
  module CST,
  module Typ
) where

-- Main core AST module
-- This module re-exports all the other AST modules
-- to be later imported by the parser and the type checker.

import Plume.Syntax.Concrete as CST
import Plume.Syntax.Common.Type as Typ
