{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Plume.TypeChecker.Monad.State where

import Control.Monad.Except
import Data.Map qualified as Map
import GHC.IO
import GHC.Records
import Plume.Syntax.Concrete (Position)
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.Monad.Type.Error
import Plume.TypeChecker.Monad.Type.Scheme

type Environment = Map Text Scheme

data CheckerState = CheckerState
  { tvarCounter :: Int
  , variables :: Environment
  , types :: Environment
  , returnType :: PlumeType
  , constraints :: Constraints
  , position :: Maybe Position
  , generics :: Map Text Int
  , extensions :: Map Extension Scheme
  }

data Constraints = Constraints
  { typesConstr :: [TypeConstraint]
  , extensionsConstr :: [TypeConstraint]
  }

data Extension = Extension
  { name :: Text
  , value :: PlumeType
  , superExtensions :: [Extension]
  }
  deriving (Show)

instance Ord Extension where
  compare (Extension n1 t1 se1) (Extension n2 t2 se2) = compare n1 n2 <> compare t1 t2 <> compare se1 se2

instance Eq Extension where
  (Extension n1 t se1) == (Extension n2 t' se2) =
    n1 == n2
      && t == t'
      && se1 == se2

checkerST :: IORef CheckerState
{-# NOINLINE checkerST #-}
checkerST = unsafePerformIO $ newIORef emptyState

emptyState :: CheckerState
emptyState =
  CheckerState
    { tvarCounter = 0
    , variables = Map.empty
    , types = Map.empty
    , returnType = TUnit
    , constraints = Constraints [] []
    , position = Nothing
    , generics = Map.empty
    , extensions = Map.empty
    }

search
  :: forall l v m k
   . ( HasField l CheckerState (Map k v)
     , MonadIO m
     , Ord k
     , MonadError (TypeError, Maybe Position) m
     )
  => k
  -> m (Maybe v)
search k = do
  v <- readIORef checkerST
  return $ Map.lookup k (getField @l v)

instance HasField "variables" CheckerState (Map Text Scheme) where
  hasField c = (\x -> c {variables = x}, variables c)

instance HasField "types" CheckerState (Map Text Scheme) where
  hasField c = (\x -> c {types = x}, types c)

instance HasField "returnType" CheckerState PlumeType where
  hasField c = (\x -> c {returnType = x}, returnType c)

instance HasField "position" CheckerState (Maybe Position) where
  hasField c = (\x -> c {position = x}, position c)

instance HasField "generics" CheckerState (Map Text Int) where
  hasField c = (\x -> c {generics = x}, generics c)

instance HasField "constraints" CheckerState Constraints where
  hasField c = (\x -> c {constraints = x}, constraints c)

instance HasField "tvarCounter" CheckerState Int where
  hasField c = (\x -> c {tvarCounter = x}, tvarCounter c)

instance HasField "extensions" CheckerState (Map Extension Scheme) where
  hasField c = (\x -> c {extensions = x}, extensions c)

instance HasField "name" Extension Text where
  hasField c = (\x -> c {name = x}, name c)

instance HasField "value" Extension PlumeType where
  hasField c = (\x -> c {value = x}, value c)

instance HasField "typesConstr" Constraints [TypeConstraint] where
  hasField c = (\x -> c {typesConstr = x}, typesConstr c)

instance HasField "extensionsConstr" Constraints [TypeConstraint] where
  hasField c = (\x -> c {extensionsConstr = x}, extensionsConstr c)