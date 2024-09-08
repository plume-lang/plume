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

withRef :: MonadIO m => IORef a -> (a -> b) -> m b
withRef ref f = liftIO $ f <$> readIORef ref