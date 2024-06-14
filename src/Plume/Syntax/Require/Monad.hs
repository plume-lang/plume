module Plume.Syntax.Require.Monad where

import GHC.IO qualified as IO
import Data.Map qualified as Map
import Data.SortedList qualified as SL
import Plume.Syntax.Concrete qualified as HLIR
import Plume.Syntax.Common.Annotation qualified as HLIR
import Plume.Syntax.Parser.Lexer
import Control.Monad.Except

type MonadResolution m = (MonadIO m, MonadError (String, HLIR.Position) m)

-- | Module unit
-- | A module unit is a file (resp. module) that contains a list of imports,
-- | a list of variables, a list of types, and a list of classes.
data ModuleUnit = MkModuleUnit
  { name :: FilePath
  , path :: FilePath
  , public :: Bool
  , imports :: [ModuleUnit]
  , -- Imported data
    variables :: Set (Text, HLIR.IsMacro)
  , types :: Set Text
  , classes :: Set Text
  , operators :: SL.SortedList CustomOperator
  }
  deriving (Show, Eq)

-- | Module state
data ModuleState = MkModuleState
  { initialPath :: FilePath
  , currentDirectory :: FilePath
  , resolved :: Map FilePath ModuleUnit
  , standardPath :: Maybe FilePath
  , boundArgs :: [Text]
  , modulePath :: Maybe FilePath
  }
  deriving (Show, Eq)

type ImportStack = [FilePath]

{-# NOINLINE importStack #-}
importStack :: IORef ImportStack
importStack = IO.unsafePerformIO . newIORef $ []

{-# NOINLINE moduleState #-}
moduleState :: IORef ModuleState
moduleState = IO.unsafePerformIO . newIORef $ 
  MkModuleState "" "" Map.empty Nothing ["#property", "#deref"] Nothing

{-# NOINLINE resultState #-}
resultState :: IORef [HLIR.Expression]
resultState = IO.unsafePerformIO . newIORef $ []

{-# NOINLINE positionStack #-}
positionStack :: IORef [HLIR.Position]
positionStack = IO.unsafePerformIO . newIORef $ []

pushPosition :: MonadIO m => HLIR.Position -> m ()
pushPosition pos = modifyIORef' positionStack (pos :)

popPosition :: MonadIO m => m HLIR.Position
popPosition = do
  stack <- readIORef positionStack
  case stack of
    [] -> error "popPosition: empty stack"
    (x:xs) -> do
      writeIORef positionStack xs
      pure x

fetchPosition :: MonadIO m => m HLIR.Position
fetchPosition = do
  stack <- readIORef positionStack
  case stack of
    [] -> error "fetchPosition: empty stack"
    (x:_) -> pure x
    