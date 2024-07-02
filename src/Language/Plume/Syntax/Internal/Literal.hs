module Language.Plume.Syntax.Internal.Literal where

import GHC.Show qualified as S

data Literal
  = MkInteger Integer
  | MkFloat Double
  | MkChar Char
  | MkString Text
  deriving (Eq, Ord)

instance Show Literal where
  show (MkInteger i) = S.show i
  show (MkFloat f) = S.show f
  show (MkChar c) = S.show c
  show (MkString s) = S.show s

instance ToText Literal where
  toText (MkInteger i) = toText (S.show i)
  toText (MkFloat f) = toText (S.show f)
  toText (MkChar c) = toText (S.show c)
  toText (MkString s) = toText (S.show s)
