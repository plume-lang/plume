{-# LANGUAGE PatternSynonyms #-}

module Plume.TypeChecker.Monad.Type where

import GHC.Show
import GHC.IO
import Prelude hiding (show)

type Level = Int

-- | Defining a meta-variable for types which represents both user-defined
-- | generics and unresolved types.
data TyVar
  = Link PlumeType
  | Unbound QuVar Level
  deriving (Eq, Show)

type QuVar = Text

data PlumeType
  = TypeVar (IORef TyVar)
  | TypeQuantified QuVar
  | TypeId Text
  | TypeApp PlumeType [PlumeType]
  deriving (Eq)

instance Show PlumeType where
  show (TypeVar ref) = do
    let v = unsafePerformIO $ readIORef ref
    case v of
      Link t -> "link(" <> show t <> ")"
      Unbound q l -> "U" <> toString q <> "-" <> show l
  show (TypeQuantified q) = "@" <> toString q
  show (args :->: ret) = "(" <> show args <> " -> " <> show ret <> ")"
  show (TypeId t) = toString t
  show (TypeApp t ts) = show t <> " " <> show ts  

-- | A type scheme is a way to quantify over types in a type system.
-- | It is used to represent polymorphic types in the type system.
type PlumeScheme = PlumeType

-- TYPE SYNONYMS SHORTCUTS

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
