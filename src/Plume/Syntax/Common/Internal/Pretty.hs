module Plume.Syntax.Common.Internal.Pretty where

import Plume.Syntax.Common
import Plume.Syntax.Internal.Pretty.ANSI
import Prettyprinter.Render.Terminal
import Prelude hiding (intercalate)

instance ANSIPretty Pattern where ansiPretty = prettyPat

instance ANSIPretty Literal where ansiPretty = prettyLit

instance ANSIPretty PlumeType where ansiPretty = prettyTy

instance ANSIPretty PlumeGeneric where ansiPretty = prettyGen

instance {-# OVERLAPS #-} ANSIPretty [PlumeGeneric] where
  ansiPretty [] = mempty
  ansiPretty gs = angles $ hsep (punctuate comma (map ansiPretty gs))

instance (ANSIPretty t) => ANSIPretty (Annotation t) where
  ansiPretty (Annotation name value) =
    anItalic (pretty name)
      <> ":" <+> ansiPretty value

typeAnnotation :: (ANSIPretty t) => Annotation (Maybe t) -> Doc AnsiStyle
typeAnnotation (Annotation name Nothing) = anItalic (pretty name)
typeAnnotation (Annotation name (Just t)) = anItalic (pretty name) <> ":" <+> ansiPretty t

prettyPat :: Pattern -> Doc AnsiStyle
prettyPat (PVariable v) = anItalic $ pretty v
prettyPat (PLiteral l) = prettyLit l
prettyPat (PConstructor n ps) = anCol Magenta (pretty n) <+> hsep (map prettyPat ps)
prettyPat PWildcard = "?"
prettyPat (PList ps slice) =
  brackets
    (hsep (punctuate comma (map prettyPat ps ++ [".." <> ansiPretty slice])))
prettyPat (PSlice p) = ".." <> pretty p

prettyLit :: Literal -> Doc AnsiStyle
prettyLit (LInt i) = anCol Yellow $ pretty i
prettyLit (LFloat f) = anCol Yellow $ pretty f
prettyLit (LString s) = anCol Green . dquotes $ pretty s
prettyLit (LBool b) = anBold $ anCol Blue (if b then "true" else "false")
prettyLit (LChar c) = anCol Green . squotes $ pretty c

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
