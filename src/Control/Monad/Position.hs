module Control.Monad.Position where

import Language.Plume.Syntax.HLIR qualified as HLIR
import GHC.IO qualified as IO

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
