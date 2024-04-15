module Plume.Syntax.Common.Pattern where

import Plume.Syntax.Common.Literal

-- | A pattern is a value that is used to match against a value
-- | It is used in pattern matching to destructure values
-- | and bind variables to parts of the value.
data Pattern
  = PVariable Text
  | PWildcard
  | PConstructor Text [Pattern]
  | PLiteral Literal
  | PList [Pattern] (Maybe Pattern)
  | PSlice Text
  deriving (Eq, Show)
