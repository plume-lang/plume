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
  pure $ s' <> s

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
findExtensionWithType n t fallback = do
  sub <- getSubst
  exts <- apply sub . Set.toList <$> gets extensions

  let extOriginTy = case t of
        (ty : _) :->: _ -> ty
        _ -> fallback

  let found =
        filter
          ( \(MkExtension n' ty (Forall _ sndTy)) ->
              n == n'
                && isRight (mgu ty extOriginTy)
                && isRight (mgu t sndTy)
          )
          exts
  case found of
    [ext'] -> pure $ Right ext'
    [] -> Left <$> throw' (NoExtensionFound n t)
    _ -> do
      let found' = map (\(MkExtension _ ty _) -> ty) found
      Left <$> throw' (MultipleExtensionsFound n found' t)

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
maxCyclicCounter = 5

resolveCyclic :: [PlumeConstraint] -> Checker (Substitution, [PlumeConstraint])
resolveCyclic cs = do
  counter <- readIORef cyclicCounter
  when (counter > maxCyclicCounter) $ do
    errors <- readIORef errorStack
    case viaNonEmpty last errors of
      Just e -> throwRaw e
      Nothing ->
        throw $
          CompilerError $
            "Cyclic constraint resolution failed after "
              <> show maxCyclicCounter
              <> " attempts"
  modifyIORef' cyclicCounter (+ 1)
  s <- gets (substitution . constraints)
  solveExtend (map (second (apply s)) cs)

solveExtend
  :: [PlumeConstraint] -> Checker (Substitution, [PlumeConstraint])
solveExtend (x@(p, DoesExtend t name funTy) : xs) = withPosition p $ do
  ext <- findExtensionWithType name funTy t
  case ext of
    Right (MkExtension _ _ sch) -> do
      s1 <- getSubst
      extFun <- apply s1 <$> instantiate sch
      let s2 = mgu funTy extFun
      case s2 of
        Right s -> do
          updateSubst s
          solveExtend (map (second $ apply s) xs)
        Left e -> throw e
    Left e@(_, err) -> do
      modifyIORef' errorStack (e :)
      (s1', cs1) <- resolveCyclic (xs <> [x])
      case cs1 of
        [] -> updateSubst s1' $> (s1', cs1)
        _ -> throw err
solveExtend (_ : xs) = solveExtend xs
solveExtend [] = do
  s <- getSubst
  pure (s, [])

solveConstraints :: Constraints -> Checker Substitution
solveConstraints cs = do
  writeIORef cyclicCounter 0
  s <- solve cs.tyConstraints
  (s', _) <- resolveCyclic cs.extConstraints
  pure $ s' <> s