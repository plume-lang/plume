module Language.Feather.CST.Literal where

-- Literals are the most basic form of expressions.
-- They are the most primary form of data in the langage, letting the user
-- express more concrete programs.

type Label = Text

data Literal
  = LInt Integer
  | LBool Bool
  | LString String
  | LChar Char
  | LFloat Double

instance ToText Literal where
  toText (LInt i) = show i
  toText (LBool b) = show b
  toText (LString s) = show s
  toText (LChar c) = show c
  toText (LFloat f) = show f
