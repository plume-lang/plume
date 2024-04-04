module Plume.TypeChecker.Monad.State where

import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.IO hiding (liftIO)
import GHC.Records
import Plume.Syntax.Concrete
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Monad.Free
import Plume.TypeChecker.Monad.Type

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

data Constraints = MkConstraints
  { tyConstraints :: [PlumeConstraint]
  , extConstraints :: [PlumeConstraint]
  , substitution :: Substitution
  }
  deriving (Eq)

data Environment = MkEnvironment
  { typeEnv :: Map Text PlumeScheme
  , datatypeEnv :: Map Text PlumeScheme
  , genericsEnv :: Map Text TyVar
  }
  deriving (Eq)

data Extension
  = MkExtension
  { extName :: Text
  , extType :: PlumeType
  , extScheme :: PlumeScheme
  }
  deriving (Eq, Ord, Show)

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

deriveHasField ''CheckState
deriveHasField ''Environment
deriveHasField ''Constraints
deriveHasField ''Extension

{-# NOINLINE checkState #-}
checkState :: IORef CheckState
checkState = unsafePerformIO $ newIORef emptyState

instance Free Extension where
  free (MkExtension _ t s) = free t <> free s

  apply s (MkExtension n t (Forall gens ty)) =
    MkExtension n (apply s t) (Forall (apply s gens) (apply s ty))

instance Free TyVar where
  free = Set.singleton

  apply s i = case Map.lookup i s of
    Just (TypeVar i') -> i'
    _ -> i