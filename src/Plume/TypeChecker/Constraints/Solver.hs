module Plume.TypeChecker.Constraints.Solver where

import GHC.IO hiding (liftIO)
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Monad
import Control.Monad.Except (MonadError(catchError))

-- | Stacking errors to provide better error messages during the constraint
-- | resolution process
{-# NOINLINE errorStack #-}
errorStack :: IORef [PlumeError]
errorStack = unsafePerformIO $ newIORef mempty

-- | Solve basic type constraints (equality and equivalence)
solve :: MonadChecker m => [PlumeConstraint] -> m ()
solve [] = pure ()
solve ((p, c) : cs) = do
  withPosition p $ solveConstraint c

  -- Solving remaining constraints
  solve cs

infix 4 `unifiesWith`

unifiesWith :: MonadChecker m => PlumeType -> PlumeType -> m ()
unifiesWith t1 t2 = do
  p <- fetchPosition
  withPosition p $ mgu t1 t2

-- infix 4 `doesExtend`

-- | Adding a extension constraint onto the constraint stack
-- doesExtend :: PlumeType -> Text -> PlumeType -> Checker ()
-- doesExtend t n a = do
--   p <- fetchPosition
--   void . withPosition p $ solveExtend [(p, DoesExtend t n a)] =<< gets extensions

-- | Solve type constraints using the `mgu` unification algorithm
solveConstraint :: MonadChecker m => TypeConstraint -> m ()
solveConstraint (t1 :~: t2) = mgu t1 t2
-- solveConstraint (DoesExtend {}) =
--   throw $ CompilerError "Only functions are supported in extensions"
solveConstraint (Hole _) =
  throw $ CompilerError "Holes are not supported yet"

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

-- | Counter to prevent infinite loop during cyclic constraint resolution
{-# NOINLINE cyclicCounter #-}
cyclicCounter :: IORef Int
cyclicCounter = unsafePerformIO $ newIORef 0

-- | Setting the maximum number of attempts to resolve cyclic constraints
-- | By default, it is set to 15
{-# INLINABLE maxCyclicCounter #-}
maxCyclicCounter :: Int
maxCyclicCounter = 15

-- | Resolve cyclic constraints
-- resolveCyclic 
--   :: [PlumeConstraint]
--   -> [Extension]
--   -> Checker [PlumeConstraint]
-- resolveCyclic cs exts = do
--   -- Checking if the cyclic maximum counter has been reached
--   counter <- readIORef cyclicCounter
--   when (counter > maxCyclicCounter) $ do
--     errors <- readIORef errorStack
--     if null errors
--       then 
--         throw $
--           CompilerError $
--             "Cyclic constraint resolution failed after "
--               <> show maxCyclicCounter
--               <> " attempts"
--       else mapM_ throwRaw errors

--   -- Incrementing the cyclic counter
--   modifyIORef' cyclicCounter (+ 1)

--   -- Solving the constraints
--   s' <- solveExtend cs exts

--   -- Resetting the cyclic counter as the resolution was successful
--   -- and updating the substitution and finally returning the result
--   writeIORef cyclicCounter 0

--   pure s'

-- -- | Type extension constraint resolution algorithm
-- solveExtend
--   :: [PlumeConstraint]
--   -> [Extension]
--   -> Checker [PlumeConstraint]
-- solveExtend (x@(p, DoesExtend _ name funTy) : xs) exts = withPosition p $ do
--   -- Trying to find an extension that matches the constraint
--   funTy' <- liftIO $ compressPaths funTy
--   ext <- findExtensionWithType name funTy' exts
--   case ext of
--     -- If an extension is found, we try to unify the function type
--     -- with the extension type
--     Right (MkExtension _ _ sch) -> do
--       (extFun, quals) <- instantiate sch
--       mgu funTy' extFun
--       solveExtend xs exts

--     -- If no extension is found, we add the error to the error stack
--     -- and try to resolve it later (using the `resolveCyclic` function)
--     Left e@(_, err) -> do
--       modifyIORef' errorStack (e:)
      
--       void $ solveExtend xs exts
--       cs1 <- resolveCyclic [x] exts

--       case cs1 of
--         -- If no constraints are left, we return the substitution
--         [] -> return cs1

--         -- Otherwise, we throw the error
--         _ -> throw err
-- solveExtend (_ : xs) exts = solveExtend xs exts
-- solveExtend [] _ = pure mempty
