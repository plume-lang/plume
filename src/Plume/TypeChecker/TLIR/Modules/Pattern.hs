module Plume.TypeChecker.TLIR.Modules.Pattern where

import Plume.Syntax.Common.Literal

data TypedPattern t
  = PVariable Text t
  | PLiteral Literal
  | PConstructor Text [TypedPattern t]
  | PSpecialVar Text t
  | PWildcard
  deriving (Eq, Show)
