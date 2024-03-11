module Plume.Syntax.Common.Internal.Pretty where

import Plume.Syntax.Common
import Plume.Syntax.Concrete.Internal.Row
import Plume.Syntax.Internal.Pretty.ANSI
import Prettyprinter.Render.Terminal
import Prelude hiding (intercalate)

instance ANSIPretty Pattern where ansiPretty = prettyPat

instance ANSIPretty Literal where ansiPretty = prettyLit

instance ANSIPretty PlumeType where ansiPretty = prettyTy

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

prettyLit :: Literal -> Doc AnsiStyle
prettyLit (LInt i) = anCol Yellow $ pretty i
prettyLit (LFloat f) = anCol Yellow $ pretty f
prettyLit (LString s) = anCol Green . dquotes $ pretty s
prettyLit (LBool b) = anBold $ anCol Blue (if b then "true" else "false")
prettyLit (LChar c) = anCol Green . squotes $ pretty c

prettyTy :: PlumeType -> Doc AnsiStyle
prettyTy (TVar t) = anBold $ pretty t
prettyTy (TId n) = anCol Magenta $ pretty n
prettyTy (TApp t ts) = prettyTy t <+> hsep (map prettyTy ts)
prettyTy (TFunction ts t) = hsep (map prettyTy ts) <+> "->" <+> prettyTy t
prettyTy (TRecord TRowEmpty) = "{}"
prettyTy (TRecord r) = ppRecord False r
prettyTy TRowEmpty = "..."
prettyTy (TRowExtend l t TRowEmpty) = pretty l <> " : " <> prettyTy t
prettyTy (TRowExtend l t r) = pretty l <> " : " <> prettyTy t <> " | " <> prettyTy r

ppRecordHelper
  :: (ANSIPretty a, IsRow a) => (Bool, Bool) -> ([Annotation a], a) -> Doc AnsiStyle
ppRecordHelper _ ([], e) = ansiPretty e
ppRecordHelper (isType, containsExtension) (names, e) =
  enclosed . indents $
    renderRows
      ( punctuate
          comma
          (map ansiPretty names)
      )
      <> rowExtends
 where
  nl :: Doc AnsiStyle
  nl = bool mempty line isType

  indents :: Doc AnsiStyle -> Doc AnsiStyle
  indents = bool id (indent 2) isType

  renderRows :: [Doc AnsiStyle] -> Doc AnsiStyle
  renderRows = bool hsep vsep isType

  rowExtends :: Doc AnsiStyle
  rowExtends = bool mempty (comma <> nl <> "..." <> ansiPretty e) containsExtension

  enclosed :: Doc AnsiStyle -> Doc AnsiStyle
  enclosed = bool (enclose "{ " " }") (enclose "{\n" "\n}") isType

ppRecord :: (ANSIPretty a, IsRow a) => Bool -> a -> Doc AnsiStyle
ppRecord b e = ppRecordHelper (b, isRowExtend e) $ extractExtend e
