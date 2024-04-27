module Plume.TypeChecker.TLIR.Modules.Pattern where

import Plume.Syntax.Common.Literal

data TypedPattern t
  = PVariable Text t
  | PLiteral Literal
  | PConstructor Text t [TypedPattern t]
  | PSpecialVar Text t
  | PWildcard t
  | PList t [TypedPattern t] (Maybe (TypedPattern t))
  deriving (Eq, Show)
