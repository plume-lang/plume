module Plume.TypeChecker.Constraints.Solver where

import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.IO
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Monad

{-# NOINLINE errorStack #-}
errorStack :: IORef [PlumeError]
errorStack = unsafePerformIO $ newIORef []

solve :: [PlumeConstraint] -> Checker Substitution
solve [] = pure Map.empty
solve ((p, c) : cs) = do
  stSub <- getSubst
  s <- withPosition p $ solveConstraint (apply stSub c)
  updateSubst s
  s' <- solve (map (second $ apply s) cs)
  let s'' = s' <> s
  updateSubst s''
  pure s''

solveConstraint :: TypeConstraint -> Checker Substitution
solveConstraint (t1 :~: t2) = case mgu t1 t2 of
  Right s -> pure s
  Left e -> throw e
solveConstraint (DoesExtend {}) =
  throw $ CompilerError "Only functions are supported in extensions"
solveConstraint (Hole _) =
  throw $ CompilerError "Holes are not supported yet"

findExtensionWithType
  :: Text -> PlumeType -> PlumeType -> Checker (Either PlumeError Extension)
findExtensionWithType n t _ = do
  sub <- getSubst
  exts <- Set.toList . applyExts sub <$> gets extensions
  let t' = apply sub t

  let found =
        filter
          ( \(MkExtension n' _ (Forall _ sndTy)) ->
              n == n'
                && isRight (mgu t' sndTy)
          )
          exts
  case found of
    [ext'] -> pure $ Right ext'
    [] -> Left <$> throw' (NoExtensionFound n t')
    _ -> do
      let found' = map (\(MkExtension _ ty _) -> ty) found
      Left <$> throw' (MultipleExtensionsFound n found' t')

isNotTVar :: PlumeType -> Bool
isNotTVar (TypeVar _) = False
isNotTVar _ = True

popConstraint :: Checker ()
popConstraint = do
  extCons <- gets (extConstraints . constraints)
  case viaNonEmpty tail extCons of
    Nothing -> pure ()
    Just xs ->
      modify $ \s ->
        s {constraints = (constraints s) {extConstraints = xs}}

throw' :: TypeError -> Checker PlumeError
throw' err = do
  pos <- fetchPosition
  pure (pos, err)

{-# NOINLINE cyclicCounter #-}
cyclicCounter :: IORef Int
cyclicCounter = unsafePerformIO $ newIORef 0

maxCyclicCounter :: Int
maxCyclicCounter = 15

resolveCyclic :: [PlumeConstraint] -> Checker (Substitution, [PlumeConstraint])
resolveCyclic cs = do
  s <- getSubst
  counter <- readIORef cyclicCounter
  when (counter > maxCyclicCounter) $ do
    errors <- readIORef errorStack
    case viaNonEmpty last errors of
      Just e -> throwRaw (second (apply s) e)
      Nothing ->
        throw $
          CompilerError $
            "Cyclic constraint resolution failed after "
              <> show maxCyclicCounter
              <> " attempts"
  modifyIORef' cyclicCounter (+ 1)
  s' <- solveExtend (map (second (apply s)) cs)
  modifyIORef' cyclicCounter (const 0)
  updateSubst (fst s')
  pure s'

solveExtend
  :: [PlumeConstraint] -> Checker (Substitution, [PlumeConstraint])
solveExtend (x@(p, DoesExtend t name funTy) : xs) = withPosition p $ do
  s1 <- getSubst
  ext <- findExtensionWithType name (apply s1 funTy) (apply s1 t)
  case ext of
    Right (MkExtension _ _ sch) -> do
      extFun <- apply s1 <$> instantiate sch
      let s2 = mgu (apply s1 funTy) extFun
      case s2 of
        Right s -> do
          let s' = s <> s1
          updateSubst s'
          solveExtend (map (second $ apply s') xs)
        Left e -> throw e
    Left e@(_, err) -> do
      modifyIORef' errorStack (e :)
      (s1', _) <- solveExtend xs
      (s2', cs1) <- resolveCyclic [second (apply s1') x]
      let s3 = s2' <> s1'
      case cs1 of
        [] -> updateSubst s3 $> (s3, cs1)
        _ -> throw (apply s3 err)
solveExtend (_ : xs) = solveExtend xs
solveExtend [] = do
  s <- getSubst
  pure (s, [])

solveConstraints :: Constraints -> Checker Substitution
solveConstraints cs = do
  writeIORef cyclicCounter 0
  s <- solve cs.tyConstraints
  (s', _) <- solveExtend (map (second $ apply s) cs.extConstraints)
  pure $ s' <> s