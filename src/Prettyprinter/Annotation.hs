module Prettyprinter.Annotation where

import Language.Plume.CST.Annotation
import Prettyprinter
import Prettyprinter.ANSI

instance {-# OVERLAPS #-} (ANSIPretty t) => ANSIPretty (Annotation (Maybe t)) where
  ansiPretty (Annotation name value) = case value of
    Nothing -> anItalic $ pretty name
    Just t -> (anItalic $ pretty name) <> ":" <+> ansiPretty t

instance (ANSIPretty t) => ANSIPretty (Annotation t) where
  ansiPretty (Annotation name value) = anItalic (pretty name) <> ":" <+> ansiPretty value
