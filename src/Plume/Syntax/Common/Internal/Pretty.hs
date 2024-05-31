module Plume.Syntax.Common.Internal.Pretty where

import Plume.Syntax.Common
import Plume.Syntax.Internal.Pretty.ANSI
import Prettyprinter.Render.Terminal
import Prelude hiding (intercalate)

instance ANSIPretty PlumeScheme where ansiPretty = prettySch

instance (ANSIPretty a, ANSIPretty (f a)) => ANSIPretty (Pattern a f) where ansiPretty = prettyPat

instance ANSIPretty Literal where ansiPretty = prettyLit

instance ANSIPretty PlumeType where ansiPretty = prettyTy

instance ANSIPretty PlumeGeneric where ansiPretty = prettyGen

instance Pretty Identifier where pretty = show

instance {-# OVERLAPS #-} ANSIPretty [PlumeGeneric] where
  ansiPretty [] = mempty
  ansiPretty gs = angles $ hsep (punctuate comma (map ansiPretty gs))

instance (ANSIPretty t) => ANSIPretty (Annotation t) where
  ansiPretty (Annotation name value _) =
    anItalic (pretty name)
      <> ":" <+> ansiPretty value

typeAnnotation :: (ANSIPretty t) => Annotation (Maybe t) -> Doc AnsiStyle
typeAnnotation (Annotation name Nothing _) = anItalic (pretty name)
typeAnnotation (Annotation name (Just t) _) = anItalic (pretty name) <> ":" <+> ansiPretty t

ppMut :: Bool -> Doc AnsiStyle
ppMut True = anCol Blue "mut "
ppMut False = mempty

argAnnotation :: (ANSIPretty t) => Annotation (Maybe t) -> Doc AnsiStyle
argAnnotation (Annotation name Nothing m) = ppMut m <> anItalic (pretty name)
argAnnotation (Annotation name (Just t) m) = ppMut m <> anItalic (pretty name) <> ":" <+> ansiPretty t

prettyPat :: (ANSIPretty a, ANSIPretty (f a)) => Pattern a f -> Doc AnsiStyle
prettyPat (PVariable v _) = anItalic $ pretty v
prettyPat (PLiteral l) = prettyLit l
prettyPat (PConstructor (n, _) ps) = anCol Magenta (pretty n) <+> hsep (map prettyPat ps)
prettyPat PWildcard{} = "?"
prettyPat (PList _ ps slice) =
  brackets
    (hsep (punctuate comma (map prettyPat ps ++ [".." <> ansiPretty slice])))
prettyPat (PSlice p _) = ".." <> pretty p
prettyPat (PSpecialVar v _) = anCol Yellow $ pretty v

prettyLit :: Literal -> Doc AnsiStyle
prettyLit (LInt i) = anCol Yellow $ pretty i
prettyLit (LFloat f) = anCol Yellow $ pretty f
prettyLit (LString s) = anCol Green . dquotes $ pretty s
prettyLit (LBool b) = anBold $ anCol Blue (if b then "true" else "false")
prettyLit (LChar c) = anCol Green . squotes $ pretty c

prettySch :: PlumeScheme -> Doc AnsiStyle
prettySch (MkScheme gs t) = angles (hsep (punctuate comma (map ansiPretty gs))) <+> "->" <+> ansiPretty t

prettyTy :: PlumeType -> Doc AnsiStyle
prettyTy (TId n) = anCol Magenta $ pretty n
prettyTy (TFunction ts t) = parens (hsep . punctuate comma $ map prettyTy ts) <+> "->" <+> prettyTy t
prettyTy (TTuple ts) = parens (hsep . punctuate comma $ map prettyTy ts)
prettyTy (TApp t ts) = prettyTy t <+> hsep (map prettyTy ts)

prettyGen :: PlumeGeneric -> Doc AnsiStyle
prettyGen (GVar n) = anCol Yellow $ "a" <> pretty n
prettyGen (GExtends n tys) =
  anCol Yellow (pretty n)
    <+> anCol Blue "extends"
    <+> hsep (punctuate comma (map pretty tys))
