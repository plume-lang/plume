module Plume.Syntax.Concrete.Internal.Pretty where

import Plume.Syntax.Concrete
import Prettyprinter
import Prettyprinter.ANSI
import Prettyprinter.Render.Terminal
import Prelude hiding (intercalate)

instance {-# OVERLAPS #-} (ANSIPretty t) => ANSIPretty (Annotation (Maybe t)) where
  ansiPretty (Annotation name value) = case value of
    Nothing -> anItalic $ pretty name
    Just t -> (anItalic $ pretty name) <> ":" <+> ansiPretty t

instance (ANSIPretty t) => ANSIPretty (Annotation t) where
  ansiPretty (Annotation name value) = anItalic (pretty name) <> ":" <+> ansiPretty value

instance {-# OVERLAPS #-} ANSIPretty Program where
  ansiPretty (d : ds) = ansiPretty d <> line <> (if length ds == 0 then "" else line) <> ansiPretty ds
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
prettyExpr _ r@(ERowExtension{}) = ppExtend (extract r)
 where
  extract :: Expression -> ([(Text, Expression)], Expression)
  extract (ERowExtension label val r') = ((label, val) : names, rest')
   where
    (names, rest') = extract r'
  extract e = ([], e)

  ppExtend :: ([(Text, Expression)], Expression) -> Doc AnsiStyle
  ppExtend ([], e) = prettyExpr 0 e
  ppExtend (names, ERowEmpty) =
    braces' . vsep $
      punctuate
        comma
        ( map
            ( \(n, v) ->
                pretty n <> ": " <> prettyExpr 0 v
            )
            names
        )
  ppExtend (names, e) =
    "{\n"
      <> ( indent 2 $
            vsep
              ( punctuate
                  comma
                  ( map
                      ( \(n, v) ->
                          pretty n <> ": " <> prettyExpr 0 v
                      )
                      names
                  )
              )
              <> comma
              <> line
              <> "..."
              <> prettyExpr 0 e
         )
      <> "\n}"
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
prettyTy (TRecord r) = ppExtend (extract r)
 where
  extract :: ConcreteType -> ([(Text, ConcreteType)], ConcreteType)
  extract (TRowExtend label val r') = ((label, val) : names, rest')
   where
    (names, rest') = extract r'
  extract e = ([], e)

  ppExtend :: ([(Text, ConcreteType)], ConcreteType) -> Doc AnsiStyle
  ppExtend ([], e) = prettyTy e
  ppExtend (names, TRowEmpty) =
    braces' . hsep $
      punctuate
        comma
        ( map
            ( \(n, v) ->
                pretty n <> ": " <> prettyTy v
            )
            names
        )
  ppExtend (names, e) =
    "{"
      <+> ( hsep
              ( punctuate
                  comma
                  ( map
                      ( \(n, v) ->
                          pretty n <> ": " <> prettyTy v
                      )
                      names
                  )
              )
              <> comma
                <+> "..."
              <> prettyTy e
          )
      <+> "}"
prettyTy TRowEmpty = "..."
prettyTy (TRowExtend l t TRowEmpty) = pretty l <> " : " <> prettyTy t
prettyTy (TRowExtend l t r) = pretty l <> " : " <> prettyTy t <> " | " <> prettyTy r

braces' :: Doc a -> Doc a
braces' = enclose "{ " " }"
