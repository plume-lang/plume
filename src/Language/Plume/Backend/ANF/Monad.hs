module Language.Plume.Backend.ANF.Monad where

import GHC.IO qualified as IO

type MonadANF m = MonadIO m

{-# NOINLINE symbolCounter #-}
symbolCounter :: IORef Int
symbolCounter = IO.unsafePerformIO $ newIORef 0

freshSymbol :: MonadANF m => Text -> m Text
freshSymbol prefix = do
  i <- liftIO $ atomicModifyIORef' symbolCounter (\i -> (i + 1, i))
  return $ prefix <> "__anf__" <> show i
