module Plume.TypeChecker.TLIR.Internal.Pretty where

import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Internal.Pretty (prettyLit)
import Plume.Syntax.Internal.Pretty.ANSI
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR
import Prettyprinter.Render.Terminal
import Prelude hiding (intercalate)

type Expression = TypedExpression PlumeType
type Pattern = TypedPattern PlumeType
type Program = [Expression]

instance ANSIPretty Expression where ansiPretty = prettyExpr

instance {-# OVERLAPS #-} ANSIPretty Program where
  ansiPretty [d] = ansiPretty d
  ansiPretty (d : ds) = ansiPretty d <> line <> line <> ansiPretty ds
  ansiPretty [] = mempty

prettyExpr :: Expression -> Doc AnsiStyle
prettyExpr (EApplication e es) =
  prettyExpr e
    <> parens (hsep . punctuate comma $ map prettyExpr es)
prettyExpr (EVariable v t) = anItalic (pretty v) <> colon <+> prettyTy t
prettyExpr (EExtVariable v t) = anCol Red (pretty v) <> colon <+> prettyTy t
prettyExpr (ELiteral l) = prettyLit l
prettyExpr (EDeclaration a generics e1' e2') =
  gen
    <> arg a
      <+> "="
      <+> prettyExpr e1'
      <+> ( case e2' of
              Nothing -> ""
              Just e2'' -> anCol Blue "\nin" <+> prettyExpr e2''
          )
 where
  gen = "forall " <> hsep (punctuate comma (map ansiPretty generics)) <> ". "
  arg (Annotation x t') = pretty x <> colon <+> prettyTy t'
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
  ppArgs xs ret = parens (hsep . punctuate comma $ map arg xs) <> ppRet ret

  ppRet t' = ":" <+> prettyTy t'

  arg (Annotation x t') = pretty x <> colon <+> prettyTy t'
prettyExpr (EBlock es) =
  line' <> indent 2 (vsep (map prettyExpr es))
prettyExpr (ELocated e _) = prettyExpr e
prettyExpr (ESwitch e ps) =
  anCol Blue "switch"
    <+> prettyExpr e
    <+> line
    <> indent 2 (vsep (map prettyCase ps))
 where
  prettyCase (p, e') = anCol Blue "case" <+> prettyPat p <+> "=>" <+> prettyExpr e'
prettyExpr (EReturn e) = anCol Blue "return" <+> prettyExpr e
prettyExpr (ENativeFunction n gens (args :->: ret)) =
  anCol Blue "native"
    <+> pretty n
    <+> angles (hsep . punctuate comma $ map ansiPretty gens)
    <+> parens (hsep . punctuate comma $ map ansiPretty args)
    <+> ":"
    <+> prettyTy ret
prettyExpr (ENativeFunction {}) = error "ENativeFunction: invalid type"

prettyPat :: Pattern -> Doc AnsiStyle
prettyPat (PVariable v t) = pretty v <> colon <+> prettyTy t
prettyPat (PLiteral l) = prettyLit l
prettyPat (PConstructor p1 p2) =
  pretty p1 <+> parens (hsep $ punctuate comma $ map prettyPat p2)
prettyPat PWildcard = anCol Blue "_"