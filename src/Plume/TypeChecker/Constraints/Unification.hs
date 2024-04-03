module Plume.TypeChecker.Constraints.Unification where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Monad

infix 4 `unifiesTo`

unifiesTo :: PlumeType -> PlumeType -> Checker ()
t1 `unifiesTo` t2 = do
  p <- fetchPosition
  pushConstraint @"tyConstraints" (p, t1 :~: t2)

createConstraint :: TypeConstraint -> Checker PlumeConstraint
createConstraint c = do
  p <- fetchPosition
  pure (p, c)

doesExtend :: PlumeType -> Text -> PlumeType -> Checker ()
doesExtend t n a = do
  p <- fetchPosition
  pushConstraint @"extConstraints" (p, DoesExtend t n a)

variable :: TyVar -> PlumeType -> Either TypeError Substitution
variable n t
  | t == TypeVar n = Right Map.empty
  | n `Set.member` free t =
      Left (InfiniteType n t)
  | otherwise = Right $ Map.singleton n t

mgu
  :: PlumeType
  -> PlumeType
  -> Either TypeError Substitution
mgu (TypeVar i) t = variable i t
mgu t (TypeVar i) = variable i t
mgu (TypeApp t1 t2) (TypeApp t3 t4) = mguMany (t1 : t2) (t3 : t4)
mgu t1@(TypeId n) t2@(TypeId n') =
  if n == n'
    then Right Map.empty
    else Left (UnificationFail t1 t2)
mgu t1 t2 = Left (UnificationFail t1 t2)

mguMany
  :: [PlumeType]
  -> [PlumeType]
  -> Either TypeError Substitution
mguMany [] [] = Right Map.empty
mguMany (t1 : t1s) (t2 : t2s) = do
  s1 <- mgu t1 t2
  s2 <- mguMany (apply s1 t1s) (apply s1 t2s)
  return $ s2 <> s1
mguMany t1s t2s = Left (UnificationMismatch t1s t2s)