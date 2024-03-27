{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Plume.TypeChecker.Monad (
  module Monad,
  MonadChecker,
  Inference,
  Result,
  fresh,
  freshTVar,
  instantiate,
  generalize,
  local,
  gets,
  extract,
  throw,
  withVariables,
  withReturnType,
  withGenerics,
  with,
  with',
) where

import Control.Monad.Except
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Records
import Plume.Syntax.Concrete (Position)
import Plume.Syntax.Translation.Generics (Spreadable)
import Plume.TypeChecker.Monad.State as Monad
import Plume.TypeChecker.Monad.Substitution as Monad
import Plume.TypeChecker.Monad.Type as Monad
import Plume.TypeChecker.Monad.Type.Error as Monad
import Plume.TypeChecker.Monad.Type.Scheme as Monad
import Text.Megaparsec
import Prelude hiding (gets, local)

type Result a = Spreadable [a] a

type MonadChecker m = (MonadIO m, MonadError (TypeError, Maybe Position) m)
type Inference m from to =
  (MonadChecker m) => from -> m (PlumeType, Result to, [Qualified])

fresh :: (MonadIO m) => m Int
fresh = liftIO $ do
  i <- (.tvarCounter) <$> readIORef checkerST
  modifyIORef' checkerST $ \s -> s {tvarCounter = i + 1}
  return i

freshTVar :: (MonadIO m) => m PlumeType
freshTVar = TVar <$> fresh

extract :: PlumeGeneric -> Int
extract (GVar n) = n
extract (GExtends n _) = n

instantiate :: (MonadIO m) => Scheme -> m (PlumeType, Substitution, [Qualified])
instantiate (Forall vars t) = do
  vars' <- mapM (const freshTVar) vars
  let s = Map.fromList $ zip vars vars'
   in case apply s t of
        qs :=>: t' -> return (t', s, qs)

local :: (MonadChecker m) => (CheckerState -> CheckerState) -> m a -> m a
local f m = do
  old <- readIORef checkerST
  modifyIORef checkerST f
  a <- m
  s <- readIORef checkerST
  writeIORef checkerST old
  modifyIORef'
    checkerST
    ( \s' ->
        s'
          { tvarCounter = s.tvarCounter
          , constraints = s.constraints
          }
    )
  return a

generalize :: Environment -> [Qualified] -> PlumeType -> Scheme
generalize env qs t = Forall vars (qs :=>: t)
 where
  vars = Set.toList ((free t <> free qs) Set.\\ free env)

gets :: (MonadIO m) => (CheckerState -> a) -> m a
gets f = f <$> readIORef checkerST

throw :: (MonadChecker m) => TypeError -> m a
throw err =
  throwError . (err,) . position =<< readIORef checkerST

withVariables :: (MonadChecker m) => [(Text, Scheme)] -> m a -> m a
withVariables vars =
  with' @"variables" (fromList vars <>)

withReturnType :: (MonadChecker m) => PlumeType -> m a -> m a
withReturnType = with @"returnType"

withGenerics :: (MonadChecker m) => [(Text, Int)] -> m a -> m a
withGenerics gens =
  with' @"generics" (fromList gens <>)

with
  :: forall l a m b
   . (MonadChecker m, HasField l CheckerState a, Semigroup a)
  => a
  -> m b
  -> m b
with v = with' @l (v <>)

with'
  :: forall l a m b
   . (MonadChecker m, HasField l CheckerState a)
  => (a -> a)
  -> m b
  -> m b
with' f m = do
  old <- readIORef checkerST
  writeIORef checkerST (setField @l old (f (getField @l old)))
  a <- m
  new <- readIORef checkerST
  writeIORef checkerST (setField @l new (getField @l old))
  return a

instance Semigroup SourcePos where
  p <> _ = p