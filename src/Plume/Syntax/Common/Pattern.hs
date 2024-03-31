module Plume.Syntax.Common.Pattern where

import Plume.Syntax.Common.Literal

data Pattern
  = PVariable Text
  | PWildcard
  | PConstructor Text [Pattern]
  | PLiteral Literal
  | PList [Pattern] (Maybe Pattern)
  | PSlice Text
  deriving (Eq, Show)
