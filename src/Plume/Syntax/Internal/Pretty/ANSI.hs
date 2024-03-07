{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Plume.Syntax.Internal.Pretty.ANSI (
  module PP,
  ANSIPretty (..),
  anCol,
  anBold,
  anItalic,
) where

import Prettyprinter as PP
import Prettyprinter.Internal as PP
import Prettyprinter.Render.Terminal

instance (ANSIPretty a) => ANSIPretty (Maybe a) where
  ansiPretty (Just a) = ansiPretty a
  ansiPretty Nothing = mempty

class ANSIPretty a where
  ansiPretty :: a -> Doc AnsiStyle
  default ansiPretty :: (Pretty a) => a -> Doc AnsiStyle
  ansiPretty = pretty

anCol :: Color -> Doc AnsiStyle -> Doc AnsiStyle
anCol = annotate . color

anBold :: Doc AnsiStyle -> Doc AnsiStyle
anBold = annotate bold

anItalic :: Doc AnsiStyle -> Doc AnsiStyle
anItalic = annotate italicized