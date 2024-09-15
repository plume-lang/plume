module Plume.TypeChecker.Monad.State where

import GHC.IO hiding (liftIO)
import GHC.Records
import Plume.Syntax.Concrete
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR qualified as Typed

type Substitution = Map Text PlumeType

-- | Type checker state
-- | The type checker state holds the current state of the type checker
-- | It stores the environment, the return type, the positions, and the natives
-- | that are available in the current scope.
data CheckState = MkCheckState
  { environment :: Environment
  , returnType :: Maybe PlumeType
  , positions :: [Position]
  , natives :: Map Text (PlumeScheme, Position)
  , isAsynchronous :: Bool
  , substitution :: Substitution
  }
  deriving (Eq)

-- | Environment
-- | The environment is used to store the type and datatype environment
-- | It is used to store the types and datatypes that are available in the
-- | current scope. It also stores the generics that are available in the
-- | scope. Additionally, it stores the extension environment and the class
-- | (resp. interface) environment.
data Environment = MkEnvironment
  { typeEnv :: Map Text PlumeScheme
  , datatypeEnv :: Map Text PlumeScheme
  , genericsEnv :: Map Text PlumeType
  , extendEnv :: ExtendEnv
  , classEnv :: ClassEnv
  , funDeps :: Map PlumeType (Map Text PlumeType)
  , directExtensions :: Map PlumeType (Set QuVar, Map Text PlumeScheme)
  }
  deriving (Eq)

-- | Class environment
-- | The class environment is used to store the classes (resp. interfaces) that
-- | are available in the current scope.
newtype ClassEnv = MkClassEnv (Map Text Class)
  deriving (Eq, Show)

-- | Class representation
-- | The class representation is used to store the class (resp. interface).
-- | A class is composed of generic variables, a qualified qualifier (i.e. the
-- | superclasses qualifying the class), and a map of methods.
data Class = MkClass 
    [QuVar] 
    (Qualified PlumeQualifier) 
    (Map Text PlumeScheme)
    (Maybe ((Int, PlumeType), (Int, PlumeType)))
  deriving (Eq, Show)

-- | Extension environment
-- | The extension environment is used to store the extensions that are available
-- | in the current scope. An extension is composed of a qualifier, representing
-- | the implemented class (resp. interface), and an instance type.
newtype ExtendEnv = MkExtendEnv [(PlumeQualifier, Instance Typed.Expression ())]
  deriving (Eq, Show)

-- | Instance representation
-- | The instance representation is used to store the instance that is available
-- | in the current scope. An instance is composed of generic variables,
-- | a qualified generic (can be a qualifier or nothing), a map of methods
-- | (where the key is the method name and the value is the method expression),
-- | and a map of types (where the key is the type name and the value is 
-- | the type).
data Instance val a = 
  MkInstance 
    [QuVar] 
    (Qualified a) 
    (Map Text val) 
    (Map Text PlumeScheme)
    Bool
  deriving (Eq, Show)

-- | Instancing the Functor typeclass for the Instance type, in order to
-- | be able to apply `void` function to the Instance type.
instance Functor (Instance val) where
  fmap f (MkInstance qs q m t b) = MkInstance qs (fmap f q) m t b

-- | Empty state
-- | The empty state is the initial state of the type checker
emptyState :: CheckState
emptyState =
  MkCheckState
    { environment = 
      MkEnvironment 
        mempty 
        mempty 
        mempty 
        (MkExtendEnv mempty) 
        (MkClassEnv mempty)
        mempty
        mempty
    , returnType = Nothing
    , positions = []
    , natives = mempty
    , isAsynchronous = False
    , substitution = mempty
    }

-- | classMapIndex is used to keep updated the index of the method in its
-- | dictionary for generic functions suchas `show` from the show interface
-- | For instance, singleton "show" 0 means that accessing to the method "show"
-- | will return the first method in the dictionary (which is actually a list).
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

-- | Current level reference, to check bounded or unbounded types
{-# NOINLINE currentLevel #-}
currentLevel :: IORef Level
currentLevel = unsafePerformIO $ newIORef 0

-- | Current symbol reference, to generate unique symbols for type variables
{-# NOINLINE currentSymbol #-}
currentSymbol :: IORef Int
currentSymbol = unsafePerformIO $ newIORef 0

-- | To keep track of the superclasses of the current class
{-# NOINLINE superclasses #-}
superclasses :: IORef [PlumeQualifier]
superclasses = unsafePerformIO $ newIORef mempty
