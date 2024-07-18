{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}

module Plume.TypeChecker.Monad.Type where

import GHC.IO
import GHC.Show
import Prelude hiding (show)

type Level = Int

-- | TyVar can represent a type that is bounded to another type or a type
-- | that is not currently resolved (unbounded).
data TyVar
  = Link PlumeType
  | Unbound QuVar Level
  deriving (Eq, Show)

-- | QuVar represents a generic type defined by the user. For instance "A" in
-- | the following example: "fn id<A>(x: A): A => x".
type QuVar = Text

-- | PlumeType represents the types of a Plume program:
-- |
-- | - TypeVar: A type variable that can be bounded to another type or not.
-- | - TypeQuantified: A generic type defined by the user.
-- | - TypeId: A type identifier (e.g. int, bool, etc.).
-- | - TypeApp: A type application (e.g. list[int], tuple[int, bool], etc.).
data PlumeType
  = TypeVar (IORef TyVar)
  | TypeQuantified QuVar
  | TypeId Text
  | TypeApp PlumeType [PlumeType]

-- | Assumption is used to represent a value linked to an extension 
-- | dictionary. 
data Assumption a = Text :>: a
  deriving (Show, Eq)

-- | A way to qualify a type. Qualifying a type means describing a type with
-- | extensions applied to it. For instance, the type "int" can be qualified
-- | with interface "numeric" because int extends numeric<int>.
data Qualified a = [PlumeQualifier] :=>: a
  deriving (Eq, Show)

-- | A qualified type with a type that is qualified with a PlumeType.
type PlumeQualified = Qualified PlumeType

-- | PlumeScheme represents a type scheme. A type scheme is a type that is
-- | quantified by a list of generic types.
-- | It is used to check if some type vars escape their scope and to better
-- | quantify the type of a scheme.
data PlumeScheme = Forall [QuVar] PlumeQualified
  deriving (Eq, Show)

-- | A plume qualifier in its most basic form, it can be a generic type defined
-- | by the user or a type that inherits a specific interface.
data PlumeQualifier
  = IsIn [PlumeType] Text
  | IsQVar QuVar
  deriving (Eq, Show)

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

pattern TVarArg :: PlumeType -> PlumeType 
pattern TVarArg t = TypeApp (TypeId "variable") [t]

pattern TAsync :: PlumeType -> PlumeType
pattern TAsync t = TypeApp (TypeId "async") [t]

-- | A way to get extension constraints from a list of qualifiers.
getQVars :: [PlumeQualifier] -> [QuVar]
getQVars [] = []
getQVars (IsQVar q : qs) = q : getQVars qs
getQVars (IsIn q _ : qs) = concatMap getQVarsFromType q <> getQVars qs

getQVarsFromType :: PlumeType -> [QuVar]
getQVarsFromType (TypeQuantified q) = [q]
getQVarsFromType _ = []

-- | A way to remove generic types from a list of qualifiers.
removeQVars :: [PlumeQualifier] -> [PlumeQualifier]
removeQVars = filter (\case IsQVar _ -> False; _ -> True)

-- SOME INSTANCES
instance Functor Qualified where
  fmap f (qs :=>: a) = qs :=>: f a

instance Show PlumeType where
  show (TypeVar ref) = do
    let v = unsafePerformIO $ readIORef ref
    case v of
      Link t -> "#" <> show t
      Unbound q l -> toString q <> "-" <> show l -- <> " (may be a generic type)"
  show (TypeQuantified q) = "q" <> toString q
  show (TypeApp (TypeId "cons") [x, _]) = "[" <> show x <> "]"
  show (TypeApp (TypeId "nil") _) = "[]"
  show (TypeApp (TypeId "tuple") ts) = "(" <> intercalate ", " (map show ts) <> ")"
  show (args :->: ret) = "(" <> show args <> " -> " <> show ret <> ")"
  show (TypeId t) = "@" <> toString t
  show (TypeApp t ts) = show t <> "<" <> intercalate ", " (map show ts) <> ">"

instance Eq PlumeType where
  TypeId t == TypeId t' = t == t'
  TypeVar ref == TypeVar ref' = unsafePerformIO $ (==) <$> readIORef ref <*> readIORef ref'
  TypeQuantified q == TypeQuantified q' = q == q'
  TypeApp t ts == TypeApp t' ts' = t == t' && ts == ts'
  _ == _ = False

instance Ord (IORef TyVar) where
  compare ref ref' = unsafePerformIO $ do
    v <- readIORef ref
    v' <- readIORef ref'
    pure $ compare v v'

deriving instance Ord TyVar
deriving instance Ord PlumeType