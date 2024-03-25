{-# LANGUAGE PatternSynonyms #-}

module Plume.TypeChecker.Monad.Type where

import Data.Text (intercalate)
import Plume.Syntax.Internal.Pretty.ANSI
import Prettyprinter.Render.Terminal
import Prelude hiding (intercalate)

data PlumeType
  = TId Text
  | TVar Int
  | TApp PlumeType [PlumeType]
  deriving (Eq, Ord, Show)

instance ToText PlumeType where
  toText (TId n) = n
  toText (TVar i) = "a" <> show i
  toText (TApp t ts) = toText t <> "<" <> intercalate ", " (map toText ts) <> ">"

instance Semigroup PlumeType where
  t <> _ = t

data PlumeGeneric
  = GVar Int
  | GExtends Int [Text]
  deriving (Eq, Show)

pattern TFunction, (:->:) :: [PlumeType] -> PlumeType -> PlumeType
pattern TFunction args ret = TApp (TApp (TId "->") [ret]) args
pattern xs :->: ret = TFunction xs ret

pattern TTuple :: [PlumeType] -> PlumeType
pattern TTuple ts = TApp (TId ",") ts

pattern TList :: PlumeType -> PlumeType
pattern TList t = TApp (TId "[]") [t]

pattern TCon :: Text -> [PlumeType] -> PlumeType
pattern TCon s ts = TApp (TId s) ts

pattern TInt, TBool, TString, TChar, TFloat, TUnit :: PlumeType
pattern TInt = TId "int"
pattern TBool = TId "bool"
pattern TString = TId "str"
pattern TChar = TId "char"
pattern TFloat = TId "float"
pattern TUnit = TTuple []

prettyTy :: PlumeType -> Doc AnsiStyle
prettyTy (TId n) = anCol Magenta $ pretty n
prettyTy (TFunction ts t) = parens (hsep . punctuate comma $ map prettyTy ts) <+> "->" <+> prettyTy t
prettyTy (TList t) = brackets $ prettyTy t
prettyTy (TTuple ts) = parens (hsep . punctuate comma $ map prettyTy ts)
prettyTy (TApp t ts) = prettyTy t <> angles (hsep . punctuate comma $ map prettyTy ts)
prettyTy (TVar i) = anCol Yellow $ "a" <> pretty i

instance ANSIPretty PlumeType where ansiPretty = prettyTy

prettyGen :: PlumeGeneric -> Doc AnsiStyle
prettyGen (GVar n) = anCol Yellow $ "a" <> pretty n
prettyGen (GExtends n tys) =
  anCol Yellow (pretty n)
    <+> anCol Blue "extends"
    <+> hsep (punctuate comma (map pretty tys))

instance ANSIPretty PlumeGeneric where ansiPretty = prettyGen