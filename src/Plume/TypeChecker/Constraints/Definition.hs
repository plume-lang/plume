module Plume.TypeChecker.Constraints.Definition where

import Plume.Syntax.Concrete (Position)
import Plume.TypeChecker.Monad.Free
import Plume.TypeChecker.Monad.Type

data TypeConstraint
  = PlumeType :~: PlumeType
  | DoesExtend PlumeType Text PlumeType
  | Hole PlumeType
  deriving (Eq, Show)

type PlumeConstraint = (Position, TypeConstraint)

instance Free TypeConstraint where
  free (t1 :~: t2) = free t1 <> free t2
  free (DoesExtend extTy _ appTy) = free extTy <> free appTy
  free (Hole t) = free t

  apply s (t1 :~: t2) = apply s t1 :~: apply s t2
  apply s (DoesExtend extTy name appTy) =
    DoesExtend (apply s extTy) name (apply s appTy)
  apply s (Hole t) = Hole $ apply s t