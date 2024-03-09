module Plume.Syntax.Abstract.Internal.Pretty where

import Plume.Syntax.Abstract
import Plume.Syntax.Concrete.Internal.Pretty hiding (prettyExpr)
import Plume.Syntax.Concrete.Internal.Row
import Plume.Syntax.Internal.Pretty.ANSI
import Prettyprinter.Render.Terminal
import Prelude hiding (intercalate)

instance ANSIPretty Expression where ansiPretty = prettyExpr 0

instance IsRow Expression where
  isRowEmpty ERowEmpty = True
  isRowEmpty _ = False

  isRowExtend (ERowExtension {}) = True
  isRowExtend _ = False

  extractExtend (ERowExtension label val r') = ((label :@: val) : names, rest')
   where
    (names, rest') = extractExtend r'
  extractExtend e = ([], e)

instance {-# OVERLAPS #-} ANSIPretty Program where
  ansiPretty [d] = ansiPretty d
  ansiPretty (d : ds) = ansiPretty d <> line <> line <> ansiPretty ds
  ansiPretty [] = mempty

prettyExpr :: Int -> Expression -> Doc AnsiStyle
prettyExpr _ (EApplication e es) =
  prettyExpr 0 e
    <> parens (hsep . punctuate comma $ map (prettyExpr 0) es)
prettyExpr _ (EVariable v) = anItalic $ pretty v
prettyExpr _ (ELiteral l) = prettyLit l
prettyExpr _ (EDeclaration a e1' e2') =
  typeAnnotation a
    <+> "="
    <+> prettyExpr 0 e1'
    <+> case e2' of
      Nothing -> ""
      Just e2'' -> anCol Blue "\nin" <+> prettyExpr 0 e2''
prettyExpr _ (EConditionBranch e1' e2' e3') =
  anCol Blue "if"
    <+> prettyExpr 0 e1'
    <+> anCol Blue "then "
    <> prettyExpr 0 e2'
    <> anCol Blue "\nelse "
    <> prettyExpr 0 e3'
prettyExpr _ (EClosure as t e) =
  ppArgs as t
    <+> "->\n"
    <+> indent 2 (prettyExpr 0 e)
 where
  ppArgs [x :@: Nothing] Nothing = pretty x
  ppArgs xs ret = parens (hsep . punctuate comma $ map typeAnnotation xs) <> ppRet ret

  ppRet Nothing = ""
  ppRet (Just t') = ":" <+> prettyTy t'
prettyExpr _ (EBlock es) =
  line <> indent 4 (vsep (map (prettyExpr 0) es))
prettyExpr _ ERowEmpty = "..."
prettyExpr _ r@(ERowExtension {}) = ppRecord True r
prettyExpr _ (ERowSelect e l) = prettyExpr 0 e <> "." <> pretty l
prettyExpr _ (ERowRestrict e l) = prettyExpr 0 e <+> anCol Blue "except" <+> pretty l
prettyExpr _ (ELocated e _) = prettyExpr 0 e
