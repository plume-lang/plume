module Language.Plume.Syntax.Internal.Position where

import Text.Megaparsec.Pos qualified as Pos

type Position = (Pos.SourcePos, Pos.SourcePos)

class Locate a where
  locate :: Position -> a -> a
