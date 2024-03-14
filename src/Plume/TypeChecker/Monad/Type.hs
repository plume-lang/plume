{-# LANGUAGE PatternSynonyms #-}

module Plume.TypeChecker.Monad.Type where

data PlumeType
  = TId Text
  | TVar Int
  | TApp PlumeType [PlumeType]

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
