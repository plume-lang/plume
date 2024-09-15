module Plume.Syntax.Common.Literal where

-- Literals are the most basic form of expressions.
-- They are the most primary form of data in the langage, letting the user
-- express more concrete programs.

type Label = Text

data Literal
  = LInt Integer
  | LBool Bool
  | LString Text
  | LChar Char
  | LFloat Double
  | LRegex Text
  deriving (Eq, Show, Ord)
