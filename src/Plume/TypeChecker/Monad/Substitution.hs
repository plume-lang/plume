module Plume.TypeChecker.Monad.Substitution where

import Data.Map qualified as M
import Data.Set qualified as S
import Plume.Syntax.Common.Annotation
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR

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

instance (Types a) => Types (TypedExpression a) where
  free = error "Not implemented"

  apply s (EVariable n t) = EVariable n (apply s t)
  apply s (EApplication e1 e2) = EApplication (apply s e1) (apply s e2)
  apply s (EClosure anns t e) = EClosure (apply s anns) (apply s t) (apply s e)
  apply s (ELocated e p) = ELocated (apply s e) p
  apply s (ESwitch e ps) = ESwitch (apply s e) (apply s ps)
  apply s (EReturn e) = EReturn (apply s e)
  apply s (EDeclaration gens ann e1 e2) = EDeclaration (apply s gens) (apply s ann) (apply s e1) (apply s e2)
  apply s (EConditionBranch e1 e2 e3) = EConditionBranch (apply s e1) (apply s e2) (apply s e3)
  apply s (EBlock es) = EBlock (apply s es)
  apply _ (ELiteral l) = ELiteral l
  apply s (ENativeFunction n gens t) = ENativeFunction n (apply s gens) (apply s t)

instance (Types a) => Types (Annotation a) where
  free = error "Not implemented"
  apply s (Annotation a t) = a :@: apply s t

instance (Types a) => Types (TypedPattern a) where
  free = error "Not implemented"
  apply s (PVariable n t) = PVariable n (apply s t)
  apply _ (PLiteral l) = PLiteral l
  apply s (PConstructor p1 p2) = PConstructor p1 (apply s p2)
  apply _ PWildcard = PWildcard

instance Types Int where
  free = mempty
  apply = const id

instance (Types a, Types b) => Types (a, b) where
  free (a, b) = free a `S.union` free b
  apply s (a, b) = (apply s a, apply s b)