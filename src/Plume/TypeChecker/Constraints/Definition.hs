module Plume.TypeChecker.Constraints.Definition where

import Data.Set qualified as S
import Plume.Syntax.Concrete (Position)
import Plume.TypeChecker.Monad

data ConstraintConstructor
  = PlumeType :~: PlumeType
  | PlumeType `Extends` Text
  | Hole PlumeType

type TypeConstraint = (Position, ConstraintConstructor)

instance Types ConstraintConstructor where
  free (t1 :~: t2) = free t1 `S.union` free t2
  free (t `Extends` _) = free t
  free (Hole t) = free t

  apply s (t1 :~: t2) = apply s t1 :~: apply s t2
  apply s (Hole t) = Hole $ apply s t
  apply s (t `Extends` c) = apply s t `Extends` c
