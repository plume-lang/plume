module Plume.Syntax.Concrete.Internal.Pretty where

import Plume.Syntax.Common
import Plume.Syntax.Common.Internal.Pretty
import Plume.Syntax.Concrete
import Plume.Syntax.Internal.Pretty.ANSI
import Prettyprinter.Render.Terminal
import Prelude hiding (intercalate)

instance {-# OVERLAPS #-} ANSIPretty Program where
  ansiPretty [d] = ansiPretty d
  ansiPretty (d : ds) = ansiPretty d <> line <> line <> ansiPretty ds
  ansiPretty [] = mempty

instance ANSIPretty Expression where ansiPretty = prettyExpr 0

instance ANSIPretty BinaryOperator where ansiPretty = prettyBin

instance ANSIPretty PrefixOperator where ansiPretty = prettyPrefix

instance ANSIPretty (TypeConstructor PlumeType) where ansiPretty = prettyTyCons

prettyBin :: BinaryOperator -> Doc AnsiStyle
prettyBin Plus = "+"
prettyBin Minus = "-"
prettyBin Times = "*"
prettyBin Division = "/"
prettyBin Mod = "%"
prettyBin Equals = "=="
prettyBin NotEquals = "!="
prettyBin GreaterThan = ">="
prettyBin LesserThan = "<="
prettyBin StrictlyGreatherThan = ">"
prettyBin StrictlyLesserThan = "<"
prettyBin And = anCol Blue "and"
prettyBin Or = anCol Blue "or"

prettyPrefix :: PrefixOperator -> Doc AnsiStyle
prettyPrefix Not = anCol Blue "not"

prettyTyCons :: TypeConstructor PlumeType -> Doc AnsiStyle
prettyTyCons (TConstructor n ts) = anItalic (pretty n) <> parens (hsep . punctuate comma $ map ansiPretty ts)
prettyTyCons (TVariable n) = anItalic (pretty n)

prettyExpr :: Int -> Expression -> Doc AnsiStyle
prettyExpr _ (EType (Annotation name gens) ts) =
  anCol Blue "type" <+> anItalic (pretty name)
    <> angles (hsep . punctuate comma $ map ansiPretty gens)
      <+> "="
    <> line
    <> indent 2 (vsep . punctuate comma $ map ansiPretty ts)
prettyExpr i (EBinary op e1' e2') =
  if i > 0
    then parens res
    else res
 where
  res = prettyExpr (i + 1) e1' <+> prettyBin op <+> prettyExpr (i + 1) e2'
prettyExpr i (EPrefix op e) =
  if i > 0
    then parens res
    else res
 where
  res = prettyPrefix op <+> prettyExpr (i + 1) e
prettyExpr _ (EApplication e es) =
  prettyExpr 0 e
    <> parens (hsep . punctuate comma $ map (prettyExpr 0) es)
prettyExpr _ (EVariable v) = anItalic $ pretty v
prettyExpr _ (ELiteral l) = ansiPretty l
prettyExpr _ (EDeclaration generics a e1' e2') =
  ansiPretty generics
    <> typeAnnotation a
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
    <> case e3' of
      Nothing -> ""
      Just e3'' -> anCol Blue "\nelse " <> prettyExpr 0 e3''
prettyExpr _ (EClosure as t e) =
  ppArgs as t
    <+> "=>\n"
    <+> indent 2 (prettyExpr 0 e)
 where
  ppArgs [x :@: Nothing] Nothing = pretty x
  ppArgs xs ret = parens (hsep . punctuate comma $ map typeAnnotation xs) <> ppRet ret

  ppRet Nothing = ""
  ppRet (Just t') = ":" <+> prettyTy t'
prettyExpr _ (EBlock es) =
  line <> indent 4 (vsep (map (prettyExpr 0) es))
prettyExpr _ (ERequire l) = anCol Blue "require" <+> anCol Green (dquotes $ pretty l)
prettyExpr _ (ELocated e _) = prettyExpr 0 e
prettyExpr _ (EMacro n e) = anCol Yellow "@" <> anCol Yellow (pretty n) <+> "=" <+> prettyExpr 0 e
prettyExpr _ (EMacroFunction n args e) =
  anCol Yellow "@"
    <> anCol Yellow (pretty n)
      <+> parens (hsep . punctuate comma $ map pretty args)
      <+> "=>"
      <+> prettyExpr 0 e
prettyExpr _ (EMacroVariable n) = anCol Yellow "@" <> anCol Yellow (pretty n)
prettyExpr _ (EMacroApplication n es) =
  anCol Yellow "@"
    <> anCol Yellow (pretty n)
    <> parens (hsep . punctuate comma $ map (prettyExpr 0) es)
prettyExpr _ (ESwitch e ps) =
  anCol Blue "switch"
    <+> prettyExpr 0 e
    <+> line
    <> indent 2 (vsep (map prettyCase ps))
 where
  prettyCase (p, e') = anCol Blue "case" <+> prettyPat p <+> "=>" <+> prettyExpr 0 e'
prettyExpr _ (EProperty n e) = parens $ prettyExpr 0 e <> "." <> anItalic (pretty n)
prettyExpr _ (EReturn e) = anCol Blue "return" <+> prettyExpr 0 e
prettyExpr _ (ETypeExtension gens a es) =
  anCol Blue "extends"
    <> angles (hsep . punctuate comma $ map ansiPretty gens)
      <+> parens (ansiPretty a)
      <+> line
    <> indent 2 (vsep (map prettyExt es))
prettyExpr _ (ENativeFunction fp n gens (args :->: ret)) =
  anCol Blue "native"
    <+> anCol Green (pretty fp)
    <+> anBold (pretty n)
    <> angles (hsep . punctuate comma $ map pretty gens)
    <> parens (hsep . punctuate comma $ map ansiPretty args)
      <+> ":"
      <+> ansiPretty ret
prettyExpr _ (ENativeFunction {}) = error "ENativeFunction: invalid type"
prettyExpr _ (EGenericProperty gens n ts t) =
  anCol Blue "property"
    <> angles (hsep . punctuate comma $ map ansiPretty gens)
      <+> anItalic (pretty n)
    <> parens (hsep . punctuate comma $ map ansiPretty ts)
      <+> ":"
      <+> ansiPretty t
prettyExpr _ (EList es) = brackets (hsep . punctuate comma $ map (prettyExpr 0) es)

prettyExt :: ExtensionMember PlumeType -> Doc AnsiStyle
prettyExt (ExtDeclaration gs a e1') =
  ansiPretty gs
    <> typeAnnotation a
      <+> "="
      <+> prettyExpr 0 e1'
