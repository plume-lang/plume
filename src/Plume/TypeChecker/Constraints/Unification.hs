module Plume.TypeChecker.Constraints.Unification where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Monad

infix 4 `unifiesTo`

-- | Adding a constraint equivalence between two types onto
-- | the constraint stack
unifiesTo :: PlumeType -> PlumeType -> Checker ()
t1 `unifiesTo` t2 = do
  p <- fetchPosition
  pushConstraint @"tyConstraints" (p, t1 :~: t2)

-- | Creating a constraint from a type constraint
createConstraint :: TypeConstraint -> Checker PlumeConstraint
createConstraint c = do
  p <- fetchPosition
  pure (p, c)

-- | Adding a extension constraint onto the constraint stack
doesExtend :: PlumeType -> Text -> PlumeType -> Checker ()
doesExtend t n a = do
  p <- fetchPosition
  pushConstraint @"extConstraints" (p, DoesExtend t n a)

-- | Unification algorithm for type variables:
-- | - If the two types are the same, return the empty substitution
-- | - If one of the types is a type variable, return a substitution
-- |   that binds the type variable to the other type. But only if the
-- |   type variable is not in the free variables of the other type.
variable :: TyVar -> PlumeType -> Either TypeError Substitution
variable n t
  | t == TypeVar n = Right Map.empty
  | n `Set.member` free t =
      Left (InfiniteType n t)
  | otherwise = Right $ Map.singleton n t

-- | Unification algorithm for two types:
-- | - If the two types are the same, return the empty substitution
-- | - If one of the types is a type variable, unify them using the
-- |   `variable` function
-- | - If the two types are type applications, unify their arguments
-- |   and their head
-- | - If the two types are type identifiers, check if they are the same
-- |   and return the empty substitution
-- | - Otherwise, return a unification error
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

-- | Unifying multiple types together.
-- | Given types must follow some conditions such as having the same length.
-- | It there are no types to unify, return the empty substitution.
-- | Otherwise, unify the first types together and then unify the rest of the
-- | types recursively using the last substitution.
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