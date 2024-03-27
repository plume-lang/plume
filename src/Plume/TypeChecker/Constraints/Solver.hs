{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Plume.TypeChecker.Constraints.Solver where

import Control.Monad.Except
import Data.List
import Data.Map qualified as M
import Plume.Syntax.Concrete (Position)
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Monad
import Prelude hiding (elem, gets, optional)

type MonadSolver m = (MonadIO m, MonadError (TypeError, Maybe Position) m)

solve
  :: (MonadSolver m)
  => [TypeConstraint]
  -> m Substitution
solve [] = return mempty
solve ((pos, t1 :~: t2) : xs) = do
  let s1 = mgu t1 t2
  case s1 of
    Left err -> throwError (err, Just pos)
    Right s1' -> do
      s2 <- solve $ map (second (apply s1')) xs
      pure $ compose s2 s1'
solve ((pos, Extends extType extFunName appTy) : xs) = do
  scheme <- solveExtension pos (extFunName, extType)
  (t, _, _) <- instantiate scheme

  let s1 = mgu t appTy
  case s1 of
    Left err -> throwError (err, Just pos)
    Right s1' -> do
      s2 <- solve $ map (second (apply s1')) xs
      return $ s1' `compose` s2
solve ((_, Hole _) : xs) = solve xs

solveExtension :: (MonadSolver m) => Position -> (Text, PlumeType) -> m Scheme
solveExtension p (name, t) = with @"position" (Just p) $ do
  findWithMgu (Extension name t [])

doesExtensionHaveGeneralOne :: (MonadSolver m) => Text -> m Bool
doesExtensionHaveGeneralOne name = do
  exts <- gets extensions

  let res =
        findWithKey
          ( \(Extension n t _) ->
              n == name
                && (case t of TVar _ -> True; _ -> False)
          )
          exts

  return $ isJust res

doesExtensionHaveGenericOne :: (MonadSolver m) => Text -> m Bool
doesExtensionHaveGenericOne name = do
  exts <- gets extensions

  let res = findWithKey (\(Extension n _ _) -> n == name) exts

  return $ isJust res

findWithMgu :: (MonadSolver m) => Extension -> m Scheme
findWithMgu e@(Extension name t1 _) = do
  exts <- gets extensions
  (_, found) <-
    findMatchingExtension
      ( \(Extension n t2 _) ->
          return (n == name && isRight (mgu t1 t2))
      )
      exts
      e

  return found

optional :: (MonadSolver m) => m a -> m (Maybe a)
optional m = catchError (Just <$> m) (\_ -> return Nothing)

findMin :: (Ord k) => Map k v -> Maybe k
findMin m = case M.keys m of
  [] -> Nothing
  xs -> Just $ minimum xs

lookupWith :: (Eq k, Ord k, Monad m) => Map k v -> (k -> m Bool) -> m (Maybe v)
lookupWith m f = do
  xs <- filterWithKeyM (\k _ -> f k) m
  let k = findMin xs
  return $ case k of
    Just k' -> M.lookup k' xs
    Nothing -> Nothing

chooseMaybeGeneric
  :: (Eq k, Ord k, Monad m) => Map k v -> (k -> m Bool) -> m (Map k v)
chooseMaybeGeneric m f =
  filterWithKeyM (\k _ -> f k) m

findWithKey
  :: (Ord k) => (k -> Bool) -> Map k a -> Maybe (k, a)
findWithKey p m = do
  let m' = M.toList m
  find (p . fst) m'

findWithKeyM
  :: (Ord k, Monad m) => (k -> a -> m Bool) -> Map k a -> m (Maybe (k, a))
findWithKeyM f m = do
  let m' = M.toList m
  findM (uncurry f) m'

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM f xs = do
  ys <- filterM f xs
  return $ case ys of
    [] -> Nothing
    y : _ -> Just y

findMatchingExtension
  :: (MonadSolver m)
  => (Extension -> m Bool)
  -> Map Extension Scheme
  -> Extension
  -> m (Extension, Scheme)
findMatchingExtension f m ext = do
  let m' = M.toList m
  case m' of
    [] -> throw (NoExtensionFound ext.name ext.value)
    (k@(Extension n _ _), v) : xs -> do
      b <- f k
      res <- filterWithKeyM (\k' _ -> f k') $ M.fromList xs
      if b
        then
          if M.null res
            then return (k, v)
            else do
              let extNames = map ((.value)) $ M.keys res
              throw (MultipleExtensionsFound n extNames ext.value)
        else findMatchingExtension f (M.fromList xs) ext

filterWithKeyM
  :: (Ord k, Monad m) => (k -> v -> m Bool) -> Map k v -> m (Map k v)
filterWithKeyM f m = do
  xs <-
    catMaybes
      <$> forM
        (M.toList m)
        ( \(k, v) -> do
            b <- f k v
            return $ if b then Just (k, v) else Nothing
        )
  return $ M.fromList xs

runSolver
  :: (MonadIO m)
  => [TypeConstraint]
  -> m (Either (TypeError, Maybe Position) Substitution)
runSolver = runExceptT . solve

dischargeExtension :: Extension -> [Extension]
dischargeExtension (Extension name t superExts) =
  let superExts' = concatMap dischargeExtension superExts
   in Extension name t [] : superExts'