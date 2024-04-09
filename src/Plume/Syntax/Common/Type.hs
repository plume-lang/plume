{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Plume.Syntax.Common.Type where

import Data.Text hiding (map)
import Prelude hiding (intercalate, unwords)

-- Types are used to represent the data types of the language.
-- This module defines concrete types and type variables which
-- will be mainly used by the type checker to infer the types of expressions.

data PlumeType
  = TId Text
  | TApp PlumeType [PlumeType]
  deriving (Eq, Show)

data PlumeGeneric
  = GVar Text
  | GExtends Text [Text]
  deriving (Eq, Show)

pattern TMut :: PlumeType -> PlumeType
pattern TMut t = TApp (TId "mut") [t]

pattern TFunction, (:->:) :: [PlumeType] -> PlumeType -> PlumeType
pattern TFunction args ret = TApp (TApp (TId "->") [ret]) args
pattern xs :->: ret = TFunction xs ret

pattern TTuple :: [PlumeType] -> PlumeType
pattern TTuple ts = TApp (TId "tuple") ts

pattern TList :: PlumeType -> PlumeType
pattern TList t = TApp (TId "list") [t]

pattern TCon :: Text -> [PlumeType] -> PlumeType
pattern TCon s ts = TApp (TId s) ts

pattern TInt, TBool, TString, TChar, TFloat, TUnit :: PlumeType
pattern TInt = TId "int"
pattern TBool = TId "bool"
pattern TString = TId "str"
pattern TChar = TId "char"
pattern TFloat = TId "float"
pattern TUnit = TId "unit"
