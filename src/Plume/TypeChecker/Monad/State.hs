module Plume.TypeChecker.Monad.State where

import GHC.IO hiding (liftIO)
import GHC.Records
import Plume.Syntax.Concrete
import Plume.TypeChecker.Monad.Type

-- | Type checker state
-- | The type checker state holds the current state of the type checker
-- | It stores the type variable counter, the environment, the generated
-- | constraints, the created extensions, the return type, the positions
-- | and the native functions
data CheckState = MkCheckState
  { environment :: Environment
  , extensions ::  [Extension]
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
  deriving (Show, Eq)

-- | Empty state
-- | The empty state is the initial state of the type checker
emptyState :: CheckState
emptyState =
  MkCheckState
    { environment = MkEnvironment mempty mempty mempty
    , extensions = mempty
    , returnType = Nothing
    , positions = []
    , natives = mempty
    }

-- | Derive the HasField instances for the CheckState, Environment, Constraints
-- | and Extension types
deriveHasField ''CheckState
deriveHasField ''Environment
deriveHasField ''Extension

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
