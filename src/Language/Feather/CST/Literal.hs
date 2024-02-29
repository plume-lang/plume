module Language.Feather.CST.Literal where

-- Literals are the most basic form of expressions.
-- They are the most primary form of data in the langage, letting the user
-- express more concrete programs.

type Label = String

data Literal
  = LInt Integer
  | LBool Bool
  | LString String
  | LChar Char
  | LFloat Double

instance Show Literal where
  show (LInt i) = show i
  show (LBool b) = show b
  show (LString s) = show s
  show (LChar c) = show c
  show (LFloat f) = show f
