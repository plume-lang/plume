module Plume.Syntax.Abstract.Internal.Pretty where

import Plume.Syntax.Abstract
import Plume.Syntax.Concrete.Internal.Pretty hiding (prettyExpr)
import Plume.Syntax.Concrete.Internal.Row
import Plume.Syntax.Internal.Pretty.ANSI
import Prettyprinter.Render.Terminal
import Prelude hiding (intercalate)

instance ANSIPretty Expression where ansiPretty = prettyExpr

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

prettyExpr :: Expression -> Doc AnsiStyle
prettyExpr (EApplication e es) =
  prettyExpr e
    <> parens (hsep . punctuate comma $ map prettyExpr es)
prettyExpr (EVariable v) = anItalic $ pretty v
prettyExpr (ELiteral l) = prettyLit l
prettyExpr (EDeclaration a e1' e2') =
  typeAnnotation a
    <+> "="
    <+> prettyExpr e1'
    <+> ( case e2' of
            Nothing -> ""
            Just e2'' -> anCol Blue "\nin" <+> prettyExpr e2''
        )
prettyExpr (EConditionBranch e1' e2' e3') =
  anCol Blue "if"
    <+> prettyExpr e1'
    <+> anCol Blue "then "
    <> prettyExpr e2'
    <> anCol Blue "\nelse "
    <> prettyExpr e3'
prettyExpr (EClosure as t e) =
  ppArgs as t
    <+> "->"
    <+> prettyExpr e
 where
  ppArgs [x :@: Nothing] Nothing = pretty x
  ppArgs xs ret = parens (hsep . punctuate comma $ map typeAnnotation xs) <> ppRet ret

  ppRet Nothing = ""
  ppRet (Just t') = ":" <+> prettyTy t'
prettyExpr (EBlock es) =
  line' <> indent 2 (vsep (map prettyExpr es))
prettyExpr ERowEmpty = "..."
prettyExpr r@(ERowExtension {}) = ppRecord True r
prettyExpr (ERowSelect e l) = prettyExpr e <> "." <> pretty l
prettyExpr (ERowRestrict e l) = prettyExpr e <+> anCol Blue "except" <+> pretty l
prettyExpr (ELocated e _) = prettyExpr e
prettyExpr (ESwitch e ps) =
  anCol Blue "switch"
    <+> prettyExpr e
    <+> line
    <> indent 2 (vsep (map prettyCase ps))
 where
  prettyCase (p, e') = anCol Blue "case" <+> prettyPat p <+> "->" <+> prettyExpr e'