module Plume.Syntax.Concrete.Internal.Pretty where

import Plume.Syntax.Concrete
import Plume.Syntax.Concrete.Internal.Row
import Plume.Syntax.Internal.Pretty.ANSI
import Prettyprinter.Render.Terminal
import Prelude hiding (intercalate)

instance {-# OVERLAPS #-} (ANSIPretty t) => ANSIPretty (Annotation (Maybe t)) where
  ansiPretty (Annotation name (Just v)) =
    anItalic (pretty name)
      <> ":" <+> ansiPretty v
  ansiPretty (Annotation name Nothing) = anItalic (pretty name)

instance (ANSIPretty t) => ANSIPretty (Annotation t) where
  ansiPretty (Annotation name value) =
    anItalic (pretty name)
      <> ":" <+> ansiPretty value

instance {-# OVERLAPS #-} ANSIPretty Program where
  ansiPretty [d] = ansiPretty d
  ansiPretty (d : ds) = ansiPretty d <> line <> line <> ansiPretty ds
  ansiPretty [] = mempty

instance ANSIPretty Expression where ansiPretty = prettyExpr 0

instance ANSIPretty Literal where ansiPretty = prettyLit

instance ANSIPretty BinaryOperator where ansiPretty = prettyBin

instance ANSIPretty PrefixOperator where ansiPretty = prettyPrefix

instance ANSIPretty ConcreteType where ansiPretty = prettyTy

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

prettyExpr :: Int -> Expression -> Doc AnsiStyle
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
prettyExpr _ (ELiteral l) = prettyLit l
prettyExpr _ (EDeclaration a e1' e2') =
  ansiPretty a
    <+> "="
    <+> prettyExpr 0 e1'
    <+> case e2' of
      Nothing -> ""
      Just e2'' -> anCol Blue "\nin" <+> prettyExpr 0 e2''
prettyExpr _ (EConditionBranch e1' e2' e3') =
  anCol Blue "if"
    <+> prettyExpr 0 e1'
    <+> anCol Blue "then\n"
    <> indent 2 (prettyExpr 0 e2')
    <> anCol Blue "\nelse\n"
    <> indent 2 (prettyExpr 0 e3')
prettyExpr _ (EClosure as t e) =
  ppArgs as t
    <+> "->\n"
    <+> indent 2 (prettyExpr 0 e)
 where
  ppArgs [x :@: Nothing] Nothing = pretty x
  ppArgs xs ret = parens (hsep . punctuate comma $ map ansiPretty xs) <> ppRet ret

  ppRet Nothing = ""
  ppRet (Just t') = ":" <+> prettyTy t'
prettyExpr _ (EBlock es) =
  braces $
    line
      <> indent 2 (vsep (map (prettyExpr 0) es))
      <> line
prettyExpr _ ERowEmpty = "..."
prettyExpr _ r@(ERowExtension{}) = ppRecord True r
prettyExpr _ (ERowSelect e l) = prettyExpr 0 e <> "." <> pretty l
prettyExpr _ (ERowRestrict e l) = prettyExpr 0 e <+> anCol Blue "except" <+> pretty l
prettyExpr _ (ELocated e _) = prettyExpr 0 e

prettyLit :: Literal -> Doc AnsiStyle
prettyLit (LInt i) = anCol Yellow $ pretty i
prettyLit (LFloat f) = anCol Yellow $ pretty f
prettyLit (LString s) = anCol Green . dquotes $ pretty s
prettyLit (LBool b) = anBold $ anCol Blue $ case b of
  True -> "true"
  False -> "false"
prettyLit (LChar c) = anCol Green . squotes $ pretty c

prettyTy :: ConcreteType -> Doc AnsiStyle
prettyTy (TVar t) = anBold $ pretty t
prettyTy (TId n) = anCol Magenta $ pretty n
prettyTy (TApp t ts) = prettyTy t <+> hsep (map prettyTy ts)
prettyTy (TFunction ts t) = hsep (map prettyTy ts) <+> "->" <+> prettyTy t
prettyTy (TRecord TRowEmpty) = "{}"
prettyTy (TRecord r) = ppRecord False r
prettyTy TRowEmpty = "..."
prettyTy (TRowExtend l t TRowEmpty) = pretty l <> " : " <> prettyTy t
prettyTy (TRowExtend l t r) = pretty l <> " : " <> prettyTy t <> " | " <> prettyTy r

ppRecordHelper :: (ANSIPretty a, IsRow a) => Bool -> ([(Text, a)], a) -> Doc AnsiStyle
ppRecordHelper _ ([], e) = ansiPretty e
ppRecordHelper b (names, e) =
  enclosed . indents $
    ( renderRows $
        punctuate
          comma
          ( map
              ( \(n, v) ->
                  pretty n <> ": " <> ansiPretty v
              )
              names
          )
    )
      <> rowExtends
 where
  nl :: Doc AnsiStyle
  nl = bool mempty line b

  indents :: Doc AnsiStyle -> Doc AnsiStyle
  indents = bool id (indent 2) b

  renderRows :: [Doc AnsiStyle] -> Doc AnsiStyle
  renderRows = bool hsep vsep b

  rowExtends :: Doc AnsiStyle
  rowExtends = bool mempty (nl <> comma <> ansiPretty e) (isRowExtend e)

  enclosed :: Doc AnsiStyle -> Doc AnsiStyle
  enclosed = bool (enclose "{ " " }") (enclose "{\n" "\n}") b

ppRecord :: (ANSIPretty a, IsRow a) => Bool -> a -> Doc AnsiStyle
ppRecord b = ppRecordHelper b . extractExtend