{-# LANGUAGE OverloadedRecordDot #-}

module Plume.TypeChecker.Monad (
  module Monad,
  MonadChecker,
  checkerST,
  fresh,
  freshTVar,
  instantiate,
  local,
) where

import Data.Map qualified as Map
import GHC.IO hiding (liftIO)
import Plume.TypeChecker.Monad.State as Monad
import Plume.TypeChecker.Monad.Substitution as Monad
import Plume.TypeChecker.Monad.Type as Monad
import Plume.TypeChecker.Monad.Type.Error as Monad
import Plume.TypeChecker.Monad.Type.Scheme as Monad
import Prelude hiding (local)

type MonadChecker m = MonadIO m

checkerST :: IORef CheckerState
{-# NOINLINE checkerST #-}
checkerST = unsafePerformIO $ newIORef emptyState

fresh :: (MonadIO m) => m Int
fresh = liftIO $ do
  i <- (.tvarCounter) <$> readIORef checkerST
  modifyIORef' checkerST $ \s -> s {tvarCounter = i + 1}
  return i

freshTVar :: (MonadIO m) => m PlumeType
freshTVar = TVar <$> fresh

instantiate :: (MonadIO m) => Scheme -> m PlumeType
instantiate (Forall vars t) = do
  vars' <- mapM (const freshTVar) vars
  let s = Map.fromList $ zip vars vars'
   in return $ apply s t

local :: (MonadChecker m) => (Environment -> Environment) -> m a -> m a
local f m = do
  old <- readIORef checkerST
  writeIORef checkerST (old {variables = f old.variables})
  a <- m
  writeIORef checkerST old
  return a