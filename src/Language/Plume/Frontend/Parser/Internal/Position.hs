module Language.Plume.Frontend.Parser.Internal.Position where

import Language.Plume.Syntax.HLIR qualified as HLIR
import Language.Plume.Frontend.Parser qualified as P

localize :: (MonadIO m, HLIR.Locate a) => P.Parser m a -> P.Parser m a
localize p = do
  start <- P.getSourcePos
  x <- p
  end <- P.getSourcePos

  pure $ HLIR.locate (start, end) x
