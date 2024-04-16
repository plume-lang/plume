module Plume.TypeChecker.Constraints.Definition where

import Plume.Syntax.Concrete (Position)
import Plume.TypeChecker.Monad.Free
import Plume.TypeChecker.Monad.Type

-- | Type constraints
-- | Used to modelize some equality and equivalence judgements between
-- | types.
-- |
-- | t1 :~: t2 <=> t1 is equivalent to t2 (meaning we could deduce t1 from t2
-- |               and vice-versa)
-- | DoesExtend extTy name appTy <=> `extTy` extends a method named `name` that 
-- |                                 shapes `appTy`
-- | Hole t <=> `t` is a hole that needs to be filled, used to help the user
-- |            to fill the blanks in the type inference process
data TypeConstraint
  = PlumeType :~: PlumeType
  | DoesExtend PlumeType Text PlumeType
  | Hole PlumeType
  deriving (Eq, Show)

-- | Plume constraint are always bound to a position for better error handling
type PlumeConstraint = (Position, TypeConstraint)

instance Free TypeConstraint where
  free (t1 :~: t2) = free t1 <> free t2
  free (DoesExtend extTy _ appTy) = free extTy <> free appTy
  free (Hole t) = free t

  apply s (t1 :~: t2) = apply s t1 :~: apply s t2
  apply s (DoesExtend extTy name appTy) =
    DoesExtend (apply s extTy) name (apply s appTy)
  apply s (Hole t) = Hole $ apply s t