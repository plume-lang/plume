module Language.Plume.Backend.LLIR.Monad where

import GHC.IO qualified as IO
import Language.Plume.Syntax.LLIR qualified as LLIR

type MonadLLIR m = MonadIO m

{-# NOINLINE resultState #-}
resultState :: IORef [LLIR.LLIR "declaration"]
resultState = IO.unsafePerformIO $ newIORef []

{-# NOINLINE symbolCounter #-}
symbolCounter :: IORef Int
symbolCounter = IO.unsafePerformIO $ newIORef 0

{-# NOINLINE globals #-}
globals :: IORef (Map Text LLIR.PlumeType)
globals = IO.unsafePerformIO $ newIORef mempty

{-# NOINLINE globalVariables #-}
globalVariables :: IORef (Set Text)
globalVariables = IO.unsafePerformIO $ newIORef mempty

{-# NOINLINE locals #-}
locals :: IORef (Map Text LLIR.PlumeType)
locals = IO.unsafePerformIO $ newIORef mempty

freshSymbol :: MonadLLIR m => Text -> m Text
freshSymbol n = do
  modifyIORef' symbolCounter (+1)
  i <- readIORef symbolCounter

  pure $ "@" <> n <> "__llir__" <> show i
