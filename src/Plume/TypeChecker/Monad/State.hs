module Plume.TypeChecker.Monad.State where

import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.IO hiding (liftIO)
import GHC.Records
import Plume.Syntax.Concrete
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Monad.Free
import Plume.TypeChecker.Monad.Type

-- | Type checker state
-- | The type checker state holds the current state of the type checker
-- | It stores the type variable counter, the environment, the generated
-- | constraints, the created extensions, the return type, the positions
-- | and the native functions
data CheckState = MkCheckState
  { nextTyVar :: Int
  , environment :: Environment
  , constraints :: Constraints
  , extensions :: Set Extension
  , returnType :: Maybe PlumeType
  , positions :: [Position]
  , natives :: Map Text (PlumeScheme, Position)
  }
  deriving (Eq)

-- | Constraints
-- | The constraints are used to store the constraints generated during the
-- | type checking process. It stores the type constraints, the extension
-- | constraints and the substitution.
-- | Types and extensions are unified using the substitution.
data Constraints = MkConstraints
  { tyConstraints :: [PlumeConstraint]
  , extConstraints :: [PlumeConstraint]
  , substitution :: Substitution
  }
  deriving (Eq)

-- | Environment
-- | The environment is used to store the type and datatype environment
-- | It is used to store the types and datatypes that are available in the
-- | current scope. It also stores the generics that are available in the
-- | scope.
data Environment = MkEnvironment
  { typeEnv :: Map Text PlumeScheme
  , datatypeEnv :: Map Text PlumeScheme
  , genericsEnv :: Map Text TyVar
  }
  deriving (Eq)

-- | Extension
-- | An extension is a piece of metadata that is attached to a type
-- | It is used to overload safely the `extType` type. 
-- | We couldn't load them onto the variable environment because there would
-- | have been conflicts with same-key elements.
-- | Additionally, we can't directly infer the good extension to use for a
-- | variable because it might be an unresolved meta-variable.
data Extension
  = MkExtension
  { extName :: Text
  , extType :: PlumeType
  , extScheme :: PlumeScheme
  }
  deriving (Eq, Ord, Show)

-- | Empty state
-- | The empty state is the initial state of the type checker
emptyState :: CheckState
emptyState =
  MkCheckState
    { nextTyVar = 0
    , environment = MkEnvironment mempty mempty mempty
    , constraints = MkConstraints mempty mempty mempty
    , extensions = mempty
    , returnType = Nothing
    , positions = []
    , natives = mempty
    }

-- | Derive the HasField instances for the CheckState, Environment, Constraints
-- | and Extension types
deriveHasField ''CheckState
deriveHasField ''Environment
deriveHasField ''Constraints
deriveHasField ''Extension

-- | Check state reference
{-# NOINLINE checkState #-}
checkState :: IORef CheckState
checkState = unsafePerformIO $ newIORef emptyState

instance Free Extension where
  free (MkExtension _ t s) = free t <> free s

  apply s (MkExtension n t (Forall gens ty)) =
    MkExtension n (apply s t) (Forall (apply s gens) (apply s ty))

-- | Apply a substitution to a set of extensions without 
-- | overwriting the for-all quantified variables
applyExts :: Substitution -> Set Extension -> Set Extension
applyExts s = Set.map applyE
 where
  applyE (MkExtension n t sc) =
    MkExtension n (apply s t) (apply s sc)

instance Free TyVar where
  free = Set.singleton

  apply s i = case Map.lookup i s of
    Just (TypeVar i') -> i'
    _ -> i