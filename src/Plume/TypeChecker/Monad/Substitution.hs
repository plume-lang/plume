module Plume.TypeChecker.Monad.Substitution where

import Data.Map qualified as M
import Data.Set qualified as S
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Concrete.Expression (TypeConstructor (..))
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

instance Types PlumeGeneric where
  apply s (GVar i) = case M.lookup i s of
    Just (TVar t) -> GVar t
    _ -> GVar i
  apply s (GExtends i ts) = case M.lookup i s of
    Just (TVar t) -> GExtends t ts
    _ -> GExtends i ts

  free (GVar i) = S.singleton i
  free (GExtends i _) = S.singleton i

instance (Types a) => Types (M.Map Text a) where
  free = free . M.elems
  apply s = M.map (apply s)

instance (Types a) => Types (TypedExpression a) where
  free (EVariable _ t) = free t
  free (EList es) = free es
  free (EApplication e1 e2) = free e1 `S.union` free e2
  free (EClosure anns t e) = free anns `S.union` free t `S.union` free e
  free (ELocated e _) = free e
  free (ESwitch e ps) = free e `S.union` free ps
  free (EReturn e) = free e
  free (EDeclaration gens ann e1 e2) = ((free ann `S.union` free e1) S.\\ free gens) `S.union` free e2
  free (EConditionBranch e1 e2 e3) = free e1 `S.union` free e2 `S.union` free e3
  free (EBlock es) = free es
  free (ELiteral _) = S.empty
  free (ENativeFunction _ gens t) = free t S.\\ free gens
  free (EExtVariable _ t) = free t
  free (EExtensionDeclaration name extTy gens args body) =
    ( free extTy
        `S.union` free args
        `S.union` free body
        `S.union` free name
    )
      S.\\ free gens
  free (EType ann ts) = free ts S.\\ free ann
  free (EEqualsType e _) = free e
  free (EAnd e1 e2) = free e1 `S.union` free e2
  free (EIndex e1 e2) = free e1 `S.union` free e2

  apply s (EVariable n t) = EVariable n (apply s t)
  apply s (EList es) = EList (apply s es)
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
  apply s (EExtVariable v t) = EExtVariable v (apply s t)
  apply s (EExtensionDeclaration name extTy gens args body) =
    EExtensionDeclaration
      name
      (apply s extTy)
      (apply s gens)
      (apply s args)
      (apply s body)
  apply s (EType ann ts) = EType (apply s ann) (apply s ts)
  apply s (EEqualsType e t) = EEqualsType (apply s e) t
  apply s (EAnd e1 e2) = EAnd (apply s e1) (apply s e2)
  apply s (EIndex e1 e2) = EIndex (apply s e1) (apply s e2)

instance (Types a) => Types (TypeConstructor a) where
  free (TConstructor _ ts) = free ts
  free (TVariable _) = S.empty

  apply s (TConstructor n ts) = TConstructor n (apply s ts)
  apply _ (TVariable n) = TVariable n

instance (Types a) => Types (Annotation a) where
  free (Annotation _ t) = free t
  apply s (Annotation a t) = a :@: apply s t

instance (Types a) => Types (TypedPattern a) where
  free (PVariable _ t) = free t
  free (PConstructor _ p2) = free p2
  free PWildcard = S.empty
  free (PLiteral _) = S.empty
  free (PSpecialVar _ t) = free t

  apply s (PVariable n t) = PVariable n (apply s t)
  apply _ (PLiteral l) = PLiteral l
  apply s (PConstructor p1 p2) = PConstructor p1 (apply s p2)
  apply _ PWildcard = PWildcard
  apply s (PSpecialVar v t) = PSpecialVar v (apply s t)

instance Types Int where
  free = mempty
  apply = const id

instance (Types a, Types b) => Types (a, b) where
  free (a, b) = free a `S.union` free b
  apply s (a, b) = (apply s a, apply s b)