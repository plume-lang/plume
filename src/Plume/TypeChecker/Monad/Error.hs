module Plume.TypeChecker.Monad.Error where

import Plume.Syntax.Concrete (Position)
import Plume.TypeChecker.Monad.Type

-- | Type errors that can be thrown by the type checker
-- | These errors are used to provide a more concise and readable error message
-- | without the pain of manually writing and formatting the error message.
-- |
-- | UnificationFail:
-- |  - The unification of two types failed, meaning that the types are not
-- |    equivalent.
-- |
-- | InfiniteType:
-- |  - The type variable is infinite, meaning that the type variable is
-- |    unbounded (e.g. `a = a -> a`).
-- |
-- | UnificationMismatch:
-- |  - The unification of two list of types failed, meaning that there may be
-- |    multiple possible types that can not be unified.
-- |
-- | EmptyMatch: There is no pattern to match in switch-case expression.
-- | UnboundVariable: The variable is undefined
-- | DuplicateNative: A native function with the same name is already defined.
-- | CompilerError: Represents an internal error in the compiler.
-- | NoReturnFound: The return type is not found within the block expression.
-- |
-- | ExhaustivenessError:
-- |  - The pattern match is not exhaustive, meaning that the pattern match does
-- |    not cover all possible cases.
-- |
-- | UnresolvedTypeVariable:
-- |  - Some type assumptions are unresolved, meaning that some types do not have
-- |    an extension for the desired interface.
-- |
-- | AlreadyDefinedInstance: The instance is already defined.
-- | ClassMismatch: The interface header mismatched with the extension header.
-- | MissingExtensionMethods: The extension does not implement all the methods.
-- | FunctionAlreadyExists: The function is already defined.

data TypeError
  = UnificationFail PlumeType PlumeType
  | InfiniteType TyVar PlumeType
  | UnificationMismatch PlumeType [PlumeType] [PlumeType]
  | EmptyMatch
  | UnboundVariable Text
  | DuplicateNative Text PlumeScheme
  | CompilerError Text
  | NoReturnFound PlumeType
  | ExhaustivenessError String
  | UnresolvedTypeVariable [Assumption PlumeType]
  | AlreadyDefinedInstance Text PlumeType
  | ClassMismatch Text PlumeQualifier PlumeQualifier
  | MissingExtensionMethods Text [Text]
  | UnknownExtensionMethods Text [Text]
  | FunctionAlreadyExists Text PlumeScheme
  deriving (Eq, Show)

type PlumeError = (Position, TypeError)

instance Exception TypeError
