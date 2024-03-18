{-# LANGUAGE TypeFamilies #-}

module Control.Monad.IO where

import Prelude hiding (modify)

-- We're using the `mtl` library to define our own monad transformer stack.
-- That's kind of the only monad we're using in this project because it's
-- simple, easy to manipulate and understand and also complete for our needs.
--
-- The goal of this monad is to represent many monadic operations in one:
-- - IO operations obviously
-- - Reader operations to pass around the environment
-- - State by using IORef for instance
-- - Error handling with throwIO function
type IOReader env a = ReaderT env IO a

delete, reset :: (Monoid a, MonadIO m) => IORef a -> m ()
delete ref = liftIO $ writeIORef ref mempty
reset = delete

initialize :: (Monoid a, MonadIO m) => m (IORef a)
initialize = liftIO $ newIORef mempty

runIOReader :: env -> IOReader env a -> IO a
runIOReader = flip runReaderT
