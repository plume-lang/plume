{-# LANGUAGE PatternSynonyms #-}

module Plume.TypeChecker.Monad.Type where

newtype TyVar = MkTyVar {unTyVar :: Int}
  deriving (Eq, Ord, Show)

data PlumeType
  = TypeVar TyVar
  | TypeId Text
  | TypeApp PlumeType [PlumeType]
  deriving (Eq, Show, Ord)

data PlumeScheme
  = Forall [TyVar] PlumeType
  deriving (Eq, Show, Ord)

pattern TMut :: PlumeType -> PlumeType
pattern TMut t = TypeApp (TypeId "mut") [t]

pattern TFunction, (:->:) :: [PlumeType] -> PlumeType -> PlumeType
pattern TFunction args ret = TypeApp (TypeApp (TypeId "->") [ret]) args
pattern xs :->: ret = TFunction xs ret

pattern TTuple :: [PlumeType] -> PlumeType
pattern TTuple ts = TypeApp (TypeId "tuple") ts

pattern TList :: PlumeType -> PlumeType
pattern TList t = TypeApp (TypeId "list") [t]

pattern TInt, TBool, TString, TChar, TFloat, TUnit :: PlumeType
pattern TInt = TypeId "int"
pattern TBool = TypeId "bool"
pattern TString = TypeId "str"
pattern TChar = TypeId "char"
pattern TFloat = TypeId "float"
pattern TUnit = TypeId "unit"
