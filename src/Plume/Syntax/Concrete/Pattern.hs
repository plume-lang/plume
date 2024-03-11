module Plume.Syntax.Concrete.Pattern where

import Plume.Syntax.Concrete.Literal

data ConcretePattern
  = PVariable Text
  | PWildcard
  | PConstructor Text [ConcretePattern]
  | PLiteral Literal
  deriving (Eq)
