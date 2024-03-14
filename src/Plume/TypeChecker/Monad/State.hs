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
  , constraints :: [TypeConstraint]
  , position :: Maybe Position
  , generics :: Map Text Int
  }

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
    , constraints = []
    , position = Nothing
    , generics = Map.empty
    }

search
  :: forall l v m
   . ( HasField l CheckerState (Map Text v)
     , MonadIO m
     , MonadError (TypeError, Maybe Position) m
     )
  => Text
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

instance HasField "constraints" CheckerState [TypeConstraint] where
  hasField c = (\x -> c {constraints = x}, constraints c)

instance HasField "tvarCounter" CheckerState Int where
  hasField c = (\x -> c {tvarCounter = x}, tvarCounter c)