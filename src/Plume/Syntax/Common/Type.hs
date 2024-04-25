{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Plume.Syntax.Common.Type where

import Data.Text hiding (map)
import Prelude hiding (intercalate, unwords)

-- | Types are used to represent the type of a value
-- | They are used in the type checker to check the types of expressions
-- | and to infer the types of expressions.
-- | They're mainly composed of two constructors which are the type identifier
-- | and the type application.
-- |
-- | example: int, list[int], tuple[int, int], ->[int, int]
data PlumeType
  = TId Text
  | TApp PlumeType [PlumeType]
  deriving (Eq, Show)

-- | A generic is a type that is used to represent a type variable
-- | It is used in the type checker to represent a type that is not known
-- | at compile time. It is used to represent polymorphic types.
-- |
-- | example: a, b, c
-- |
-- | GExtends may be used in the future releases to describe a generic
-- | behavior for an extended type.
-- | example: a extends show => a must implement the show type extension
data PlumeGeneric
  = GVar Text
  | GExtends Text [Text]
  deriving (Eq, Show)

getGenericName :: PlumeGeneric -> Text
getGenericName (GVar name) = name
getGenericName (GExtends name _) = name

-- TYPE SYNONYMS SHORTCUTS

-- | A type that represents a mutable reference to another type
-- | example: mut int, mut list[int]
pattern TMut :: PlumeType -> PlumeType
pattern TMut t = TApp (TId "mut") [t]

-- | A type that represents a function type
-- | example: fn(int, int): int <=> (->[int])[int, int]
pattern TFunction, (:->:) :: [PlumeType] -> PlumeType -> PlumeType
pattern TFunction args ret = TApp (TApp (TId "->") [ret]) args
pattern xs :->: ret = TFunction xs ret

-- | A type that represents a tuple type
-- | example: (int, int) <=> tuple[int, int]
pattern TTuple :: [PlumeType] -> PlumeType
pattern TTuple ts = TApp (TId "tuple") ts

-- | A type that represents a list type
-- | example: [int] <=> list[int]
pattern TList :: PlumeType -> PlumeType
pattern TList t = TApp (TId "list") [t]

-- | A type that represents a type constructor
-- | example: C<int, int> <=> C[int, int]
pattern TCon :: Text -> [PlumeType] -> PlumeType
pattern TCon s ts = TApp (TId s) ts

-- | Some primitive types encoded as type identifiers
pattern TInt, TBool, TString, TChar, TFloat, TUnit :: PlumeType
pattern TInt = TId "int"
pattern TBool = TId "bool"
pattern TString = TId "str"
pattern TChar = TId "char"
pattern TFloat = TId "float"
pattern TUnit = TId "unit"
