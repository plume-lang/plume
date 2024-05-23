module Plume.TypeChecker.Constraints.Solver where

import GHC.IO hiding (liftIO)
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Monad
import Control.Monad.Except (MonadError(catchError))

-- | Stacking errors to provide better error messages during the constraint
-- | resolution process
{-# NOINLINE errorStack #-}
errorStack :: IORef [PlumeError]
errorStack = unsafePerformIO $ newIORef mempty

infix 4 `unifiesWith`

-- | Solving a type constraint using the `mgu` unification algorithm
-- | Constraint type of the form `t1 :~: t2`
unifiesWith :: MonadChecker m => PlumeType -> PlumeType -> m ()
unifiesWith t1 t2 = flip withPosition (mgu t1 t2) =<< fetchPosition

doesThrowError :: MonadChecker m => m a -> m Bool
doesThrowError action = do
  catchError (action >> pure False) (\_ -> pure True)

-- | Check if a type is not a type variable
isNotTVar :: PlumeType -> IO Bool
isNotTVar (TypeVar tv) = do
  tv' <- readIORef tv
  case tv' of
    Link t -> isNotTVar t
    Unbound _ _ -> pure False
isNotTVar _ = pure True

-- | Throw a type error with the current position without adding
-- | it to the error stack
throw' :: MonadChecker m => TypeError -> m PlumeError
throw' err = do
  pos <- fetchPosition
  pure (pos, err)
