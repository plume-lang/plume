{-# LANGUAGE PatternSynonyms #-}

module Language.Plume.Syntax.Internal.Type where

import Data.String qualified as S
import GHC.Show qualified as S
import GHC.IO qualified as IO

-- Level represents the level of a type variable. It is used to determine the
-- scope of a type variable.
type Level = Int

-- QuVar represents a generic type defined by the user. For instance "A" in
-- the following example: "fn id<A>(x: A): A => x".
type QuVar = Text

-- Plume types
-- A type in Plume consists either of a type identifier, a type variable, or a
-- type application.
-- For example: int, a, list[int] are represented as MkTyId "int", MkTyVar "a",
-- and MkTyApp (MkTyId "list") [MkTyInt] respectively.
data PlumeType
  = MkTyId Text
  | MkTyVar (IORef TyVar)
  | MkTyApp PlumeType [PlumeType]
  | MkTyExists QuVar PlumeType
  deriving (Eq)

-- Type variable represents a type variable in Plume. It can either be a link to
-- another type or an unbound type variable.
data TyVar
  = Link PlumeType
  | Unbound QuVar Level
  deriving (Eq)

-- Type scheme represents a type quantified over a set of type variables.
-- For example: ∀A. A -> A is represented as:
--   MkTyScheme ["A"] (MkTyId "A" :->: Mk
data PlumeScheme = MkTyScheme [QuVar] PlumeType
  deriving Show

-- Function arrow type
-- First argument is the list of argument types, and the second argument is the
-- return type.
-- For example: fn(int, int): int is represented as [int, int] :->: int
pattern (:->:) :: [PlumeType] -> PlumeType -> PlumeType
pattern (:->:) xs ret = MkTyApp (MkTyApp (MkTyId "->") [ret]) xs

pattern MkTyClosure :: PlumeType -> PlumeType
pattern MkTyClosure ty = MkTyApp (MkTyId "__closure__") [ty]

-- Primitive types
pattern MkTyInt, MkTyFloat, MkTyChar, MkTyString, MkTyUnit, MkTyBool, MkTyAny :: PlumeType
pattern MkTyInt = MkTyId "int"
pattern MkTyFloat = MkTyId "float"
pattern MkTyChar = MkTyId "char"
pattern MkTyString = MkTyId "str"
pattern MkTyUnit = MkTyId "unit"
pattern MkTyBool = MkTyId "bool"
pattern MkTyAny = MkTyId "any"

-- Compound type constructor
-- First argument is the name of the type constructor, and the second argument
-- is the list of type arguments.
-- For example: list[int] is represented as MkTyComp "list" [MkTyInt]
pattern MkTyComp :: Text -> [PlumeType] -> PlumeType
pattern MkTyComp name args = MkTyApp (MkTyId name) args

pattern MkTyTuple :: [PlumeType] -> PlumeType
pattern MkTyTuple args = MkTyComp "tuple" args

instance Show PlumeType where
  show (MkTyId idtf) = toString idtf
  show (MkTyVar var) = do
    let var' = IO.unsafePerformIO $ readIORef var
    case var' of
      Link ty -> show ty
      Unbound name _ -> "#" <> toString name
  show (xs :->: ret) =
    "(" <> S.unwords (map show xs) <> " -> " <> show ret <> ")"
  show (MkTyApp ty args) =
    "(" <> show ty <> " " <> S.unwords (map show args) <> ")"
  show (MkTyExists name ty) = "∃" <> toString name <> ". " <> show ty
