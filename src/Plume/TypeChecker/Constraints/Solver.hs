{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
      let s2 = solve $ map (second (apply s1')) xs
      compose s1' <$> s2
solve ((pos, Extends extType extFunName appTy) : xs) = do
  scheme <- solveExtension pos (extFunName, extType)
  (t, _, qs) <- instantiate scheme

  let s1 = mgu t appTy
  case s1 of
    Left err -> throwError (err, Just pos)
    Right s1' -> do
      let qs' = nub qs
      let extConstraints =
            map
              ( \case
                  ty `Has` n -> (pos, ExtensionExists n ty)
              )
              (apply s1' qs')

      s <- solve extConstraints
      s2 <- solve $ map (second (apply s1')) xs
      return $ s1' `compose` s2 `compose` s
solve ((_, Hole _) : xs) = solve xs
solve ((pos, ExtensionExists name t) : xs) = do
  sch <- solveExtension pos (name, t)
  (t', _, qs) <- instantiate sch
  case t' of
    ((extTy : _) :->: _) -> do
      let s1 = mgu t extTy
      case s1 of
        Left err -> throwError (err, Just pos)
        Right s1' -> do
          let qs' = nub qs
          let extConstraints =
                map
                  ( \case
                      ty `Has` n -> (pos, ExtensionExists n ty)
                  )
                  (apply s1' qs')
          s <- solve extConstraints
          let s' = s `compose` s1'
          s2 <- solve $ map (second (apply s')) xs
          return $ s2 `compose` s'
    _ -> throwError (NoExtensionFound name t, Just pos)

solveExtension :: (MonadSolver m) => Position -> (Text, PlumeType) -> m Scheme
solveExtension p (name, t) = with @"position" (Just p) $ do
  isGeneric <- isTypeExtensionGeneric (name, t)
  findWithMgu (Extension name t isGeneric [])

isTypeExtensionGeneric :: (MonadSolver m) => (Text, PlumeType) -> m Bool
isTypeExtensionGeneric (name, t) = do
  let genericError = throw (NoGenericExtensionFound t)
  case t of
    TVar n -> do
      exts' <- search @"extendedGenerics" n
      case exts' of
        Just tys ->
          unless (name `elem` tys) genericError
        Nothing ->
          whenM (doesExtensionHaveGeneralOne name) genericError
      return (isJust exts')
    _ -> return False

doesExtensionHaveGeneralOne :: (MonadSolver m) => Text -> m Bool
doesExtensionHaveGeneralOne name = do
  exts <- gets extensions

  let res =
        findWithKey
          ( \(Extension n t b _) ->
              n == name
                && (case t of TVar _ -> True; _ -> False)
                && not b
          )
          exts

  return $ isJust res

doesExtensionHaveGenericOne :: (MonadSolver m) => Text -> m Bool
doesExtensionHaveGenericOne name = do
  exts <- gets extensions

  let res = findWithKey (\(Extension n _ isGeneric _) -> n == name && isGeneric) exts

  return $ isJust res

findWithMgu :: (MonadSolver m) => Extension -> m Scheme
findWithMgu e@(Extension name t1 isGeneric1 _) = do
  exts <- gets extensions
  (_, found) <-
    findMatchingExtension
      ( \(Extension n t2 isGeneric2 _) -> do
          case (t1, t2) of
            (TVar _, TVar _)
              | isGeneric2 && isGeneric1 -> do
                  return (n == name && isRight (mgu t1 t2))
            _
              | not isGeneric2 && not isGeneric1 ->
                  return (n == name && isRight (mgu t1 t2))
            _ -> return False
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
    (k@(Extension n _ _ _), v) : xs -> do
      b <- f k
      res <- filterWithKeyM (\k' _ -> f k') $ M.fromList xs
      if b
        then
          if M.null res
            then return (k, v)
            else do
              let extNames = map ((.value)) $ M.keys res
              throw (MultipleExtensionsFound n extNames)
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
runSolver xs =
  runExceptT
    ( do
        s1 <- solve xs
        extCons <- nub <$> gets extensionConstraints
        compose s1 <$> solve (map (second $ apply s1) extCons)
    )

dischargeExtension :: Extension -> [Extension]
dischargeExtension (Extension name t isGen superExts) =
  let superExts' = concatMap dischargeExtension superExts
   in Extension name t isGen [] : superExts'