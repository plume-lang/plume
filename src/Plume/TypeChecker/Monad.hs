{-# LANGUAGE OverloadedRecordDot #-}

module Plume.TypeChecker.Monad where

import Data.Map qualified as Map
import GHC.IO hiding (liftIO)
import Plume.TypeChecker.Monad.State
import Plume.TypeChecker.Monad.Substitution
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.Monad.Type.Scheme

type MonadChecker m = MonadIO m

type State = IORef CheckerState

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
