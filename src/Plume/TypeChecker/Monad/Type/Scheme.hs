module Plume.TypeChecker.Monad.Type.Scheme where

import Data.Map qualified as M
import Data.Set qualified as S
import Plume.TypeChecker.Monad.Substitution
import Plume.TypeChecker.Monad.Type

data Scheme
  = Forall [Int] PlumeType

instance Types Scheme where
  free (Forall vars t) = free t `S.difference` S.fromList vars
  apply s (Forall vars t) = Forall vars (apply (foldr M.delete s vars) t)
