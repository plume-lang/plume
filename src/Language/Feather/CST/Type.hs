{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Feather.CST.Type where

import Data.Text hiding (map)
import Prelude hiding (intercalate, unwords)

-- Types are used to represent the data types of the language.
-- This module defines concrete types and type variables which
-- will be mainly used by the type checker to infer the types of expressions.

data ConcreteType
  = TId Text
  | TVar Text
  | TApp ConcreteType [ConcreteType]
  | TRecord ConcreteType
  | TRowEmpty
  | TRowExtend Text ConcreteType ConcreteType

pattern TFunction, (:->:) :: [ConcreteType] -> ConcreteType -> ConcreteType
pattern TFunction args ret = TApp (TId "->") (ret : args)
pattern xs :->: ret = TFunction xs ret

pattern TTuple :: [ConcreteType] -> ConcreteType
pattern TTuple ts = TApp (TId ",") ts

pattern TList :: ConcreteType -> ConcreteType
pattern TList t = TApp (TId "[]") [t]

pattern TCon :: Text -> [ConcreteType] -> ConcreteType
pattern TCon s ts = TApp (TId s) ts

pattern TInt, TBool, TString, TChar, TFloat, TUnit :: ConcreteType
pattern TInt = TId "int"
pattern TBool = TId "bool"
pattern TString = TId "str"
pattern TChar = TId "char"
pattern TFloat = TId "float"
pattern TUnit = TTuple []

instance ToText ConcreteType where
  toText (TId n) = n
  toText (TFunction args ret) = "(" <> unwords (map toText args) <> ") -> " <> toText ret
  toText (TTuple ts) = "(" <> intercalate ", " (map toText ts) <> ")"
  toText (TList t) = "[" <> toText t <> "]"
  toText (TVar s) = s
  toText (TCon s ts) = s <> "<" <> intercalate ", " (map toText ts) <> ">"
  toText (TApp t ts) = toText t <> "<" <> intercalate ", " (map toText ts) <> ">"
  toText (TRecord r) = "{" <> toText r <> "}"
  toText TRowEmpty = "..."
  toText (TRowExtend l t r) = l <> " : " <> toText t <> " | " <> toText r
