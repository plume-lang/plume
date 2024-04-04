module Plume.TypeChecker.Monad.Error where

import Control.Monad.Exception
import Data.Text qualified as T
import Plume.Syntax.Concrete (Position)
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR.Internal.Pretty ()
import Text.Megaparsec

data TypeError
  = -- Unification related errors
    UnificationFail PlumeType PlumeType
  | InfiniteType TyVar PlumeType
  | UnificationMismatch [PlumeType] [PlumeType]
  | -- Extensions related errors
    NoExtensionFound Text PlumeType
  | MultipleExtensionsFound Text [PlumeType] PlumeType
  | -- Expression related errors
    EmptyMatch
  | UnboundTypeVariable Int
  | UnboundVariable Text
  | DuplicateNative Text PlumeScheme
  | -- Internal errors
    CompilerError Text
  deriving (Eq, Show)

instance Throwable PlumeType

instance Throwable TyVar where
  showError (MkTyVar i) = "t" <> show i

instance (Throwable a) => Throwable [a] where
  showError = T.intercalate ", " . map showError

instance Throwable TypeError where
  showError (UnificationFail t1 t2) =
    "Cannot unify " <> showError t1 <> " with " <> showError t2
  showError (InfiniteType i t) =
    "Infinite type " <> showError i <> " in " <> showError t
  showError (UnificationMismatch ts1 ts2) =
    "Missing arguments with " <> showError ts1 <> " compared to " <> showError ts2
  showError (NoExtensionFound e t) =
    "No extension named " <> show e <> " found for type " <> showError t
  showError (MultipleExtensionsFound e ts t) =
    "Multiple extensions "
      <> show e
      <> " found for type "
      <> showError t
      <> ": "
      <> showError ts
  showError EmptyMatch = "Empty match"
  showError (UnboundTypeVariable i) = "Unbound type variable " <> show i
  showError (UnboundVariable v) = "Unbound variable " <> show v
  showError (CompilerError e) = "Compiler error: " <> e
  showError (DuplicateNative n s) =
    "Native function " <> showError (n, s) <> " already defined"

instance Throwable (Text, PlumeScheme) where
  showError (n, Forall vs t) = n <> "<" <> showError vs <> ">: " <> showError t

type PlumeError = (Position, TypeError)

instance Throwable PlumeError where
  showError (p, e) = showError p <> ": " <> showError e

instance Throwable SourcePos where
  showError p = show l <> ":" <> show c
   where
    l = unPos $ sourceLine p
    c = unPos $ sourceColumn p

instance Throwable Position where
  showError (p1, _) = fileName <> ":" <> showError p1
   where
    fileName = fromString $ sourceName p1