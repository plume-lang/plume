{-# LANGUAGE PatternSynonyms #-}

module Language.Feather.CST.Type where

import Data.List

-- Types are used to represent the data types of the language.
-- This module defines concrete types and type variables which
-- will be mainly used by the type checker to infer the types of expressions.

data ConcreteType
  = TId String
  | TVar String
  | TApp ConcreteType [ConcreteType]
  | TRecord ConcreteType
  | TRowEmpty
  | TRowExtend String ConcreteType ConcreteType

pattern TFunction, (:->:) :: [ConcreteType] -> ConcreteType -> ConcreteType
pattern TFunction args ret = TApp (TId "->") (ret : args)
pattern xs :->: ret = TFunction xs ret

pattern TTuple :: [ConcreteType] -> ConcreteType
pattern TTuple ts = TApp (TId ",") ts

pattern TList :: ConcreteType -> ConcreteType
pattern TList t = TApp (TId "[]") [t]

pattern TCon :: String -> [ConcreteType] -> ConcreteType
pattern TCon s ts = TApp (TId s) ts

pattern TInt, TBool, TString, TChar, TFloat, TUnit :: ConcreteType
pattern TInt = TId "int"
pattern TBool = TId "bool"
pattern TString = TId "str"
pattern TChar = TId "char"
pattern TFloat = TId "float"
pattern TUnit = TTuple []

instance Show ConcreteType where
  show (TId n) = n
  show (TFunction args ret) = "(" ++ unwords (map show args) ++ ") -> " ++ show ret
  show (TTuple ts) = "(" ++ intercalate ", " (map show ts) ++ ")"
  show (TList t) = "[" ++ show t ++ "]"
  show (TVar s) = s
  show (TCon s ts) = s ++ "<" ++ intercalate ", " (map show ts) ++ ">"
  show (TApp t ts) = show t ++ "<" ++ intercalate ", " (map show ts) ++ ">"
  show (TRecord r) = "{" ++ show r ++ "}"
  show TRowEmpty = "..."
  show (TRowExtend l t r) = l ++ " : " ++ show t ++ " | " ++ show r
