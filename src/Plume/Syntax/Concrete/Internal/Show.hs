module Plume.Syntax.Concrete.Internal.Show where

import Plume.Syntax.Concrete.Expression

instance ToText BinaryOperator where
  toText Plus = "+"
  toText Minus = "-"
  toText Times = "*"
  toText Division = "/"
  toText Mod = "%"
  toText Equals = "=="
  toText NotEquals = "!="
  toText GreaterThan = ">="
  toText LesserThan = "<="
  toText StrictlyGreatherThan = ">"
  toText StrictlyLesserThan = "<"
  toText And = "and"
  toText Or = "or"
  toText BinarySlice = "slice"

instance ToText PrefixOperator where
  toText Not = "not"
  toText PrefixSlice = "slice"

instance ToText PostfixOperator where
  toText PostfixSlice = "slice"