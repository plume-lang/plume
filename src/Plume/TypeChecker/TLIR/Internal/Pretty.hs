module Plume.TypeChecker.TLIR.Internal.Pretty where

import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Internal.Pretty (prettyLit)
import Plume.Syntax.Concrete.Expression (TypeConstructor (..))
import Plume.Syntax.Internal.Pretty.ANSI
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR
import Prettyprinter.Render.Terminal
import Prelude hiding (intercalate)

instance ANSIPretty Expression where ansiPretty = prettyExpr

instance {-# OVERLAPS #-} ANSIPretty Program where
  ansiPretty [d] = ansiPretty d
  ansiPretty (d : ds) = ansiPretty d <> line <> line <> ansiPretty ds
  ansiPretty [] = mempty

prettyTy :: PlumeType -> Doc AnsiStyle
prettyTy (TypeVar (MkTyVar n)) = anCol Yellow ("t" <> pretty n)
prettyTy (TypeId n) = anItalic (pretty n)
prettyTy (args :->: ret) =
  parens (hsep . punctuate comma $ map prettyTy args)
    <+> anCol Blue "->"
    <+> prettyTy ret
prettyTy (TTuple ts) = parens (hsep . punctuate comma $ map prettyTy ts)
prettyTy (TList t) = brackets (prettyTy t)
prettyTy (TypeApp t ts) = prettyTy t <> parens (hsep . punctuate comma $ map prettyTy ts)

instance ANSIPretty PlumeType where ansiPretty = prettyTy

prettyTyCons :: TypeConstructor PlumeType -> Doc AnsiStyle
prettyTyCons (TConstructor n ts) = anItalic (pretty n) <> parens (hsep . punctuate comma $ map prettyTy ts)
prettyTyCons (TVariable n) = anItalic (pretty n)

prettyExpr :: Expression -> Doc AnsiStyle
prettyExpr (EType name ts) =
  anCol Blue "type"
    <+> anItalic (pretty name)
    <+> "="
    <> line
    <> indent 2 (vsep . punctuate comma $ map prettyTyCons ts)
prettyExpr (EApplication e es) =
  prettyExpr e
    <> parens (hsep . punctuate comma $ map prettyExpr es)
prettyExpr (EVariable v t) = anItalic (pretty v) <> colon <+> prettyTy t
prettyExpr (EExtVariable n t _) = parens (anCol Red (pretty n) <> ":" <+> prettyTy t)
prettyExpr (ELiteral l) = prettyLit l
prettyExpr (EEqualsType e t) = anCol Blue "typeof" <+> prettyExpr e <+> "==" <+> pretty t
prettyExpr (EAnd e1 e2) = prettyExpr e1 <+> anCol Blue "and" <+> prettyExpr e2
prettyExpr (EIndex e1 e2) = prettyExpr e1 <> brackets (prettyExpr e2)
prettyExpr (EDeclaration a e1' e2') =
  arg a
    <+> "="
    <+> prettyExpr e1'
    <+> ( case e2' of
            Nothing -> ""
            Just e2'' -> anCol Blue "\nin" <+> prettyExpr e2''
        )
 where
  arg (Annotation x t') = pretty x <> colon <+> prettyTy t'
prettyExpr (EMutDeclaration a e1' e2') =
  anCol Blue "mut"
    <+> arg a
    <+> "="
    <+> prettyExpr e1'
    <+> ( case e2' of
            Nothing -> ""
            Just e2'' -> anCol Blue "\nin" <+> prettyExpr e2''
        )
 where
  arg (Annotation x t') = pretty x <> colon <+> prettyTy t'
prettyExpr (EMutUpdate a e1' e2') =
  anCol Blue "mut"
    <+> arg a
    <+> "="
    <+> prettyExpr e1'
    <+> ( case e2' of
            Nothing -> ""
            Just e2'' -> anCol Blue "\nin" <+> prettyExpr e2''
        )
 where
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
prettyExpr (ENativeFunction fp n (args :->: ret)) =
  anCol Blue "native"
    <+> pretty fp
    <+> pretty n
    <+> parens (hsep . punctuate comma $ map prettyTy args)
    <+> ":"
    <+> prettyTy ret
prettyExpr (ENativeFunction {}) = error "ENativeFunction: invalid type"
prettyExpr (EList es) = brackets (hsep $ punctuate comma $ map prettyExpr es)
prettyExpr (EExtensionDeclaration name extTy args body) =
  anCol Blue "extension"
    <+> pretty name
    <+> prettyTy extTy
    <+> parens (ansiPretty args)
    <+> "="
    <+> prettyExpr body

instance ANSIPretty Pattern where ansiPretty = prettyPat

prettyPat :: Pattern -> Doc AnsiStyle
prettyPat (PVariable v t) = pretty v <> colon <+> prettyTy t
prettyPat (PLiteral l) = prettyLit l
prettyPat (PConstructor p1 p2) =
  pretty p1 <+> parens (hsep $ punctuate comma $ map prettyPat p2)
prettyPat PWildcard = anCol Blue "_"
prettyPat (PSpecialVar v t) = anCol Red (pretty v) <> colon <+> prettyTy t
prettyPat (PList ps sl) =
  brackets
    ( hsep $
        punctuate comma $
          map prettyPat ps <> [".." <> ansiPretty sl]
    )