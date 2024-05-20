module Plume.TypeChecker.Monad.Error where

import Control.Monad.Exception
import Data.Text qualified as T
import Plume.Syntax.Concrete (Position)
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR.Internal.Pretty ()
import Text.Megaparsec

-- | Type errors that can be thrown by the type checker
-- | These errors are used to provide a more concise and readable error message
-- | without the pain of manually writing and formatting the error message.
-- |
-- | UnificationFail: The unification of two types failed, meaning that the two 
-- |                 types are not "equivalent".
-- | InfiniteType: A type that unifies with an other type containing the type
-- |              itself: t ~ t -> t (what should t be?)
-- | UnificationMismatch: The number of arguments given to a function does not
-- |                     match the number of arguments expected.
-- | NoExtensionFound: No extension was found for a specific type.
-- | MultipleExtensionsFound: Multiple extensions were found for a specific type.
-- | EmptyMatch: The match expression is empty.
-- | UnboundTypeVariable: A type variable is unbound.
-- | UnboundVariable: A variable is unbound.
-- | DuplicateNative: A native function is already defined.
-- | CompilerError: An internal compiler error that should not happen.
data TypeError
  = UnificationFail PlumeType PlumeType
  | InfiniteType TyVar PlumeType
  | UnificationMismatch PlumeType [PlumeType] [PlumeType]
  | NoExtensionFound Text PlumeType
  | MultipleExtensionsFound Text [PlumeType] PlumeType
  | EmptyMatch
  | UnboundTypeVariable Int
  | UnboundVariable Text
  | DuplicateNative Text PlumeScheme
  | CompilerError Text
  | NoReturnFound PlumeType
  | DeclarationReturn Text
  | ExhaustivenessError String
  | UnresolvedTypeVariable [Assumption PlumeType]
  | AlreadyDefinedInstance Text PlumeType
  | ClassMismatch Text PlumeQualifier PlumeQualifier
  | MissingExtensionMethods Text [Text]
  | FunctionAlreadyExists Text PlumeScheme
  deriving (Eq, Show)

-- THROWABLE INSTANCES FOR TYPE ERROR

instance Throwable PlumeType

instance Throwable TyVar where
  showError (Link i) = "t" <> show i
  showError (Unbound i _) = "@" <> show i

instance (Throwable a) => Throwable [a] where
  showError = T.intercalate ", " . map showError

instance Throwable PlumeScheme where
  showError (Forall qs (cs :=>: t)) = "∀" <> qs' <> showError cs <> " => " <> showError t
    where qs' = T.intercalate ", " $ map showError qs

instance Throwable PlumeQualifier where
  showError (IsIn t1 t2) = show t1 <> " extends " <> show t2
  showError (IsQVar t) = show t

instance Throwable TypeError where
  showError (UnificationFail t1 t2) =
    "Cannot unify " <> showError t1 <> " with " <> showError t2
  showError (InfiniteType i t) =
    "Infinite type " <> showError i <> " in " <> showError t
  showError (UnificationMismatch _ ts1 ts2) =
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
  showError (NoReturnFound t) = "No return found for type " <> showError t
  showError DeclarationReturn {} = "Declaration return"
  showError ExhaustivenessError {} = "Exhaustiveness error"
  showError (UnresolvedTypeVariable as) =
    "Unresolved type variable: " <> showError as
  showError (AlreadyDefinedInstance n t) =
    "Instance " <> show n <> " already defined for " <> showError t
  showError (ClassMismatch n q1 q2) =
    "Class mismatch for " <> show n <> ": " <> showError q1 <> " and " <> showError q2
  showError (MissingExtensionMethods n ms) = "Missing methods for " <> show n <> ": " <> showError ms
  showError (FunctionAlreadyExists n s) = "Function " <> show n <> " already exists with " <> showError s

instance Throwable a => Throwable (Assumption a) where
  showError (n :>: a) = show n <> ": " <> showError a

instance Throwable a => Throwable (Text, a) where
  showError (n, e) = n <> ": " <> showError e

type PlumeError = (Position, TypeError)

instance Exception TypeError

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