module Plume.Syntax.Common.Pattern where

import Plume.Syntax.Common.Literal

-- | A pattern is a value that is used to match against a value
-- | It is used in pattern matching to destructure values
-- | and bind variables to parts of the value.
data Pattern t f
  = PVariable Text (f t)
  | PWildcard (f t)
  | PConstructor (Text, f t) [Pattern t f]
  | PLiteral Literal
  | PList (f t) [Pattern t f] (Maybe (Pattern t f))
  | PSlice Text (f t)
  | PSpecialVar Text (f t)
  deriving (Eq, Show)
