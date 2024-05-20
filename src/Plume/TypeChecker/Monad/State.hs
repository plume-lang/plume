module Plume.TypeChecker.Monad.State where

import GHC.IO hiding (liftIO)
import GHC.Records
import Plume.Syntax.Concrete
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR qualified as Typed

-- | Type checker state
-- | The type checker state holds the current state of the type checker
-- | It stores the type variable counter, the environment, the generated
-- | constraints, the created extensions, the return type, the positions
-- | and the native functions
data CheckState = MkCheckState
  { environment :: Environment
  , returnType :: Maybe PlumeType
  , positions :: [Position]
  , natives :: Map Text (PlumeScheme, Position)
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
  , genericsEnv :: Map Text PlumeType
  , extendEnv :: ExtendEnv
  , classEnv :: ClassEnv
  }
  deriving (Eq)

newtype ClassEnv = MkClassEnv (Map Text Class)
  deriving (Eq, Show)

data Class = MkClass [QuVar] (Qualified PlumeQualifier) (Map Text PlumeScheme)
  deriving (Eq, Show)

newtype ExtendEnv = MkExtendEnv [(PlumeQualifier, Instance Typed.Expression ())]
  deriving (Eq, Show)

data Instance val a = MkInstance [QuVar] (Qualified a) (Map Text val) (Map Text PlumeScheme)
  deriving (Eq, Show)

instance Functor (Instance val) where
  fmap f (MkInstance qs q m t) = MkInstance qs (fmap f q) m t

-- | Empty state
-- | The empty state is the initial state of the type checker
emptyState :: CheckState
emptyState =
  MkCheckState
    { environment = MkEnvironment mempty mempty mempty (MkExtendEnv mempty) (MkClassEnv mempty)
    , returnType = Nothing
    , positions = []
    , natives = mempty
    }

{-# NOINLINE classMapIndex #-}
classMapIndex :: IORef (Map Text Int)
classMapIndex = unsafePerformIO $ newIORef mempty

-- | Derive the HasField instances for the CheckState, Environment, Constraints
-- | and Extension types
deriveHasField ''CheckState
deriveHasField ''Environment

-- | Check state reference
{-# NOINLINE checkState #-}
checkState :: IORef CheckState
checkState = unsafePerformIO $ newIORef emptyState

{-# NOINLINE currentLevel #-}
currentLevel :: IORef Level
currentLevel = unsafePerformIO $ newIORef 0

{-# NOINLINE currentSymbol #-}
currentSymbol :: IORef Int
currentSymbol = unsafePerformIO $ newIORef 0

{-# NOINLINE superclasses #-}
superclasses :: IORef [PlumeQualifier]
superclasses = unsafePerformIO $ newIORef mempty
