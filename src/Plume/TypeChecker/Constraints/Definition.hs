module Plume.TypeChecker.Constraints.Definition where

import Data.Set qualified as S
import Plume.Syntax.Concrete (Position)
import Plume.TypeChecker.Monad.Substitution
import Plume.TypeChecker.Monad.Type

data ConstraintConstructor
  = PlumeType :~: PlumeType
  | Extends PlumeType Text PlumeType
  | Hole PlumeType
  | ExtensionExists Text PlumeType
  deriving (Show)

infix 4 :~:

type TypeConstraint = (Position, ConstraintConstructor)

instance Types ConstraintConstructor where
  free (t1 :~: t2) = free t1 `S.union` free t2
  free (Extends c _ ty) = free c <> free ty
  free (Hole t) = free t
  free (ExtensionExists _ t) = free t

  apply s (t1 :~: t2) = apply s t1 :~: apply s t2
  apply s (Hole t) = Hole $ apply s t
  apply s (Extends c t ty) = Extends (apply s c) t (apply s ty)
  apply s (ExtensionExists t ty) = ExtensionExists t (apply s ty)
