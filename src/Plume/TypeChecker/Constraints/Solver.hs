module Plume.TypeChecker.Constraints.Solver where

import Control.Monad.Except
import Data.List
import Data.Map qualified as M
import Plume.Syntax.Concrete (Position)
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Monad
import Prelude hiding (gets)

solve
  :: (MonadIO m, MonadError (TypeError, Position) m)
  => [TypeConstraint]
  -> m Substitution
solve [] = return mempty
solve ((pos, t1 :~: t2) : xs) = do
  let s1 = mgu t1 t2
  case s1 of
    Left err -> throwError (err, pos)
    Right s1' -> do
      let s2 = solve $ map (second (apply s1')) xs
      compose s1' <$> s2
solve ((pos, Extends extType extFunName appTy) : xs) = do
  exts <- gets extensions
  let foundScheme = findWithMgu exts (Extension extFunName extType)
  case foundScheme of
    Right scheme -> do
      t <- instantiate scheme
      let s1 = mgu t appTy
      case s1 of
        Left err -> throwError (err, pos)
        Right s1' -> do
          s2 <- solve $ map (second (apply s1')) xs
          return $ s1' `compose` s2
    Left err -> throwError (err, pos)
solve ((_, Hole _) : xs) = solve xs

findWithMgu :: Map Extension Scheme -> Extension -> Either TypeError Scheme
findWithMgu exts (Extension name t1) = do
  found <-
    lookupWith
      exts
      ( \(Extension n t2) -> do
          if n == name
            then do
              let s = mgu t1 t2
              case s of
                Left _ -> return False
                Right _ -> return True
            else return False
      )
  case found of
    Just scheme -> return scheme
    Nothing -> Left $ CompilerError "Extension not found"

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
  :: (MonadIO m) => [TypeConstraint] -> m (Either (TypeError, Position) Substitution)
runSolver = runExceptT . solve
