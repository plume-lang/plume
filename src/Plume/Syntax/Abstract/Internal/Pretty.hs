module Plume.Syntax.Abstract.Internal.Pretty where

import Plume.Syntax.Abstract
import Plume.Syntax.Common
import Plume.Syntax.Common.Internal.Pretty
import Plume.Syntax.Concrete.Internal.Pretty ()
import Plume.Syntax.Internal.Pretty.ANSI
import Prettyprinter.Render.Terminal
import Prelude hiding (intercalate)
import Control.Monad.Exception (compilerError)

instance ANSIPretty Expression where ansiPretty = prettyExpr

instance ANSIPretty (ExtensionMember PlumeType) where
  ansiPretty = prettyExtMember

instance {-# OVERLAPS #-} ANSIPretty Program where
  ansiPretty [d] = ansiPretty d
  ansiPretty (d : ds) = ansiPretty d <> line <> line <> ansiPretty ds
  ansiPretty [] = mempty

prettyExpr :: Expression -> Doc AnsiStyle
prettyExpr (EType (Annotation name gens) ts) =
  anCol Blue "type" <+> anItalic (pretty name)
    <> angles (hsep . punctuate comma $ map ansiPretty gens)
      <+> "="
    <> line
    <> indent 2 (vsep . punctuate comma $ map ansiPretty ts)
prettyExpr (EApplication e es) =
  prettyExpr e
    <> parens (hsep . punctuate comma $ map prettyExpr es)
prettyExpr (EVariable v) = anItalic $ pretty v
prettyExpr (ELiteral l) = prettyLit l
prettyExpr (EDeclaration isMut generics a e1' e2') =
  anCol Blue "let"
    <+> (if isMut then anCol Blue "mut" else mempty)
    <+> ansiPretty generics
    <> typeAnnotation a
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
    <> case e3' of
      Nothing -> ""
      Just e3'' -> anCol Blue "\nelse " <> prettyExpr e3''
prettyExpr (EClosure as t e) =
  ppArgs as t
    <+> "=>"
    <+> prettyExpr e
 where
  ppArgs [x :@: (Nothing, mt)] Nothing = ppMut mt <+> pretty x
  ppArgs xs ret = parens (hsep . punctuate comma $ map argAnnotation xs) <> ppRet ret

  ppRet Nothing = ""
  ppRet (Just t') = ":" <+> prettyTy t'
prettyExpr (EUnMut e) = anBold "*" <> prettyExpr e
prettyExpr (EBlock es) =
  anCol Blue "block" <> line' <> indent 2 (vsep (map prettyExpr es))
prettyExpr (ELocated e _) = prettyExpr e
prettyExpr (ESwitch e ps) =
  anCol Blue "switch"
    <+> prettyExpr e
    <+> line
    <> indent 2 (vsep (map prettyCase ps))
 where
  prettyCase (p, e') = anCol Blue "case" <+> prettyPat p <+> "=>" <+> prettyExpr e'
prettyExpr (EReturn e) = anCol Blue "return" <+> prettyExpr e
prettyExpr (ETypeExtension gens ann ems) =
  anCol Blue "extends"
    <> angles (hsep . punctuate comma $ map ansiPretty gens)
      <+> parens (ansiPretty ann)
      <+> line
    <> indent 2 (vsep (map prettyExtMember ems))
prettyExpr (ENativeFunction fp n gens (args :->: ret) _) =
  anCol Blue "native"
    <+> anCol Green (pretty fp)
    <+> pretty n
    <+> angles (hsep . punctuate comma $ map pretty gens)
    <+> parens (hsep . punctuate comma $ map ansiPretty args)
    <+> ":"
    <+> prettyTy ret
prettyExpr (ENativeFunction {}) = compilerError "ENativeFunction: invalid type"
prettyExpr (EGenericProperty gens n ts t) =
  anCol Blue "property"
    <+> ansiPretty gens
    <+> pretty n
    <+> angles (hsep . punctuate comma $ map ansiPretty ts)
    <+> ":"
    <+> prettyTy t
prettyExpr (EList es) = brackets (hsep . punctuate comma $ map prettyExpr es)

prettyExtMember :: ExtensionMember PlumeType -> Doc AnsiStyle
prettyExtMember (ExtDeclaration g ann e) =
  ansiPretty g
    <> typeAnnotation ann
      <+> "="
      <+> prettyExpr e
