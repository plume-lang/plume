module Plume.Syntax.Internal.Pretty.ANSI where

import Prettyprinter
import Prettyprinter.Render.Terminal

class ANSIPretty a where
  ansiPretty :: a -> Doc AnsiStyle

instance (ANSIPretty a) => ANSIPretty [a] where
  ansiPretty = hsep . map ansiPretty

instance (ANSIPretty a) => ANSIPretty (Maybe a) where
  ansiPretty Nothing = mempty
  ansiPretty (Just a) = ansiPretty a

anCol :: Color -> Doc AnsiStyle -> Doc AnsiStyle
anCol = annotate . color

anBold :: Doc AnsiStyle -> Doc AnsiStyle
anBold = annotate bold

anItalic :: Doc AnsiStyle -> Doc AnsiStyle
anItalic = annotate italicized