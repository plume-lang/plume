{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Plume.Syntax.Internal.Pretty.ANSI (
  module PP,
  ANSIPretty (..),
  anCol,
  anBold,
  anItalic,
  anColDull,
) where

import Prettyprinter as PP
import Prettyprinter.Internal as PP
import Prettyprinter.Render.Terminal

instance (ANSIPretty a) => ANSIPretty (Maybe a) where
  ansiPretty (Just a) = ansiPretty a
  ansiPretty Nothing = mempty

instance (ANSIPretty a, ANSIPretty b) => ANSIPretty (a, b) where
  ansiPretty (a, b) = hsep $ punctuate comma [ansiPretty a, ansiPretty b]

class ANSIPretty a where
  ansiPretty :: a -> Doc AnsiStyle
  default ansiPretty :: (Pretty a) => a -> Doc AnsiStyle
  ansiPretty = pretty

anCol :: Color -> Doc AnsiStyle -> Doc AnsiStyle
anCol = annotate . color

anColDull :: Color -> Doc AnsiStyle -> Doc AnsiStyle
anColDull = annotate . colorDull

anBold :: Doc AnsiStyle -> Doc AnsiStyle
anBold = annotate bold

anItalic :: Doc AnsiStyle -> Doc AnsiStyle
anItalic = annotate italicized

instance ToText (Doc AnsiStyle) where
  {-# INLINE toText #-}
  toText = renderStrict . layoutPretty defaultLayoutOptions
