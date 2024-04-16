module Plume.TypeChecker.Constraints.Solver where

import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.IO
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Monad

-- | Stacking errors to provide better error messages during the constraint
-- | resolution process
{-# NOINLINE errorStack #-}
errorStack :: IORef (Set PlumeError)
errorStack = unsafePerformIO $ newIORef mempty

-- | Solve basic type constraints (equality and equivalence)
solve :: [PlumeConstraint] -> Checker Substitution
solve [] = pure Map.empty
solve ((p, c) : cs) = do
  -- Get the current substitution to apply it to the constraint
  -- that need to be solved 
  stSub <- getSubst
  s <- withPosition p $ solveConstraint (apply stSub c)

  -- Updating the substitution with the new one from solved constraint
  updateSubst s

  -- Solving remaining constraints
  s' <- solve (map (second $ apply s) cs)

  -- And re-updating the substitution with the new one merged
  let s'' = s' <> s
  updateSubst s''
  pure s''

-- | Solve type constraints using the `mgu` unification algorithm
solveConstraint :: TypeConstraint -> Checker Substitution
solveConstraint (t1 :~: t2) = case mgu t1 t2 of
  Right s -> pure s
  Left e -> throw e
solveConstraint (DoesExtend {}) =
  throw $ CompilerError "Only functions are supported in extensions"
solveConstraint (Hole _) =
  throw $ CompilerError "Holes are not supported yet"

-- | Find an extension with a specific type without throwing
-- | any error.
findExtensionWithType
  :: Text -> PlumeType -> PlumeType -> Checker (Either PlumeError Extension)
findExtensionWithType n t _ = do
  -- Getting the current substitution to apply it to the extension list
  sub <- getSubst
  exts <- Set.toList . applyExts sub <$> gets extensions
  let t' = apply sub t

  -- Finding the extension with the same name and type that unifies
  -- with the given type
  let found =
        filter
          ( \(MkExtension n' _ (Forall _ sndTy)) ->
              n == n'
                && isRight (mgu t' sndTy)
          )
          exts
  case found of
    -- If a single extension is found, return it
    [ext'] -> pure $ Right ext'

    -- If no extension is found, return an error
    [] -> Left <$> throw' (NoExtensionFound n t')

    -- Otherwise, it means that multiple extensions were found
    _ -> do
      let found' = map (\(MkExtension _ ty _) -> ty) found
      Left <$> throw' (MultipleExtensionsFound n found' t')

-- | Check if a type is not a type variable
isNotTVar :: PlumeType -> Bool
isNotTVar (TypeVar _) = False
isNotTVar _ = True

-- | Throw a type error with the current position without adding
-- | it to the error stack
throw' :: TypeError -> Checker PlumeError
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
resolveCyclic :: [PlumeConstraint] -> Checker (Substitution, [PlumeConstraint])
resolveCyclic cs = do
  s <- getSubst

  -- Checking if the cyclic maximum counter has been reached
  counter <- readIORef cyclicCounter
  when (counter > maxCyclicCounter) $ do
    errors <- readIORef errorStack
    if Set.null errors
      then 
        throw $
          CompilerError $
            "Cyclic constraint resolution failed after "
              <> show maxCyclicCounter
              <> " attempts"
      else mapM_ throwRaw errors

  -- Incrementing the cyclic counter
  modifyIORef' cyclicCounter (+ 1)

  -- Solving the constraints
  s' <- solveExtend (map (second (apply s)) cs)

  -- Resetting the cyclic counter as the resolution was successful
  -- and updating the substitution and finally returning the result
  modifyIORef' cyclicCounter (const 0)
  updateSubst (fst s')
  pure s'

-- | Type extension constraint resolution algorithm
solveExtend
  :: [PlumeConstraint] -> Checker (Substitution, [PlumeConstraint])
solveExtend (x@(p, DoesExtend t name funTy) : xs) = withPosition p $ do
  s1 <- getSubst

  -- Trying to find an extension that matches the constraint
  ext <- findExtensionWithType name funTy t
  case ext of
    -- If an extension is found, we try to unify the function type
    -- with the extension type
    Right (MkExtension _ _ sch) -> do
      extFun <- apply s1 <$> instantiate sch
      let s2 = mgu (apply s1 funTy) extFun
      case s2 of
        Right s -> do
          let s' = s <> s1
          updateSubst s'

          -- Solving the remaining constraints
          solveExtend (map (second $ apply s') xs)
        Left e -> throw e

    -- If no extension is found, we add the error to the error stack
    -- and try to resolve it later (using the `resolveCyclic` function)
    Left e@(_, err) -> do
      modifyIORef' errorStack (Set.insert e)
      
      (s1', _) <- solveExtend xs
      (s2', cs1) <- resolveCyclic [second (apply s1') x]

      let s3 = s2' <> s1'
      case cs1 of
        -- If no constraints are left, we return the substitution
        [] -> updateSubst s3 $> (s3, cs1)

        -- Otherwise, we throw the error
        _ -> throw (apply s3 err)
solveExtend (_ : xs) = solveExtend xs
solveExtend [] = do
  s <- getSubst
  pure (s, [])

-- | Constraint solving main function
solveConstraints :: Constraints -> Checker Substitution
solveConstraints cs = do
  -- Resetting the cyclic counter
  writeIORef cyclicCounter 0

  -- Solving the type constraints in the first place
  s <- solve cs.tyConstraints

  -- Solving the extension constraints using the substitution
  -- from the type constraints
  (s', _) <- solveExtend (map (second $ apply s) cs.extConstraints)

  -- Merging the two substitutions together
  pure $ s' <> s