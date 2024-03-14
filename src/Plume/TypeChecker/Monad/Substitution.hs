module Plume.TypeChecker.Monad.Substitution where

import Data.Map qualified as M
import Data.Set qualified as S
import Plume.TypeChecker.Monad.Type

type Substitution = M.Map Int PlumeType

class Types a where
  apply :: Substitution -> a -> a
  free :: a -> S.Set Int

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = M.map (apply s1) s2 `M.union` s1

instance (Types a) => Types [a] where
  apply s = map (apply s)
  free = foldr (S.union . free) S.empty

instance (Types a) => Types (Maybe a) where
  apply s = fmap (apply s)
  free = maybe S.empty free

instance (Types a, Ord a) => Types (S.Set a) where
  apply s = S.map (apply s)
  free = foldr (S.union . free) S.empty

instance Types PlumeType where
  apply s (TVar i) = M.findWithDefault (TVar i) i s
  apply s (TApp t1 t2) = TApp (apply s t1) (apply s t2)
  apply _ t = t

  free (TVar i) = S.singleton i
  free (TApp t1 t2) = free t1 `S.union` free t2
  free _ = S.empty

instance (Types a) => Types (M.Map Text a) where
  free = free . M.elems
  apply s = M.map (apply s)
