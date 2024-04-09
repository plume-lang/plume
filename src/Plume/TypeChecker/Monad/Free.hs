module Plume.TypeChecker.Monad.Free where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Plume.Syntax.Common.Annotation
import Plume.TypeChecker.Monad.Error
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR

type Substitution = Map TyVar PlumeType

instance {-# OVERLAPPING #-} Semigroup Substitution where
  s1 <> s2 = Map.map (apply s1) s2 `Map.union` s1

class Free a where
  free :: a -> Set TyVar
  apply :: Substitution -> a -> a

instance (Free a) => Free [a] where
  free = foldMap free
  apply = map . apply

instance (Free a) => Free (Maybe a) where
  free = foldMap free
  apply s = fmap (apply s)

instance (Free a) => Free (Annotation a) where
  free (Annotation _ t) = free t
  apply s (Annotation n t) = Annotation n (apply s t)

instance (Free a, Free b) => Free (a, b) where
  free (a, b) = free a <> free b
  apply s (a, b) = (apply s a, apply s b)

instance Free PlumeType where
  free (TypeVar v) = Set.singleton v
  free (TypeId _) = mempty
  free (TypeApp f xs) = free f <> foldMap free xs

  apply s (TypeVar v) = Map.findWithDefault (TypeVar v) v s
  apply _ t@(TypeId _) = t
  apply s (TypeApp f xs) = TypeApp (apply s f) (map (apply s) xs)

instance Free PlumeScheme where
  free (Forall vs t) = free t Set.\\ Set.fromList vs
  apply s (Forall vs t) = Forall vs (apply (foldr Map.delete s vs) t)

instance (Free t) => Free (TypedExpression t) where
  free _ = mempty

  apply s (EVariable n t) = EVariable n (apply s t)
  apply s (EApplication f xs) = EApplication (apply s f) (apply s xs)
  apply s (EClosure args ret body) = EClosure (apply s args) (apply s ret) (apply s body)
  apply s (EConditionBranch cond then' else') = EConditionBranch (apply s cond) (apply s then') (apply s else')
  apply _ (EType n ts) = EType n ts
  apply s (EEqualsType e t) = EEqualsType (apply s e) t
  apply s (EAnd e1 e2) = EAnd (apply s e1) (apply s e2)
  apply s (EIndex e1 e2) = EIndex (apply s e1) (apply s e2)
  apply s (EDeclaration a e1 e2) = EDeclaration (apply s a) (apply s e1) (apply s e2)
  apply s (EMutDeclaration a e1 e2) = EMutDeclaration (apply s a) (apply s e1) (apply s e2)
  apply s (EMutUpdate a e1 e2) = EMutUpdate (apply s a) (apply s e1) (apply s e2)
  apply _ (ELiteral l) = ELiteral l
  apply s (EExtVariable n t t') = EExtVariable n (apply s t) (apply s t')
  apply s (EList xs) = EList (apply s xs)
  apply s (EBlock xs) = EBlock (apply s xs)
  apply s (ELocated e p) = ELocated (apply s e) p
  apply s (ESwitch e cs) = ESwitch (apply s e) (apply s cs)
  apply s (EReturn e) = EReturn (apply s e)
  apply s (ENativeFunction fp n t) = ENativeFunction fp n (apply s t)
  apply s (EExtensionDeclaration n t arg body) = EExtensionDeclaration n (apply s t) (apply s arg) (apply s body)

instance (Free t) => Free (TypedPattern t) where
  free _ = mempty
  apply s (PVariable n t) = PVariable n (apply s t)
  apply s (PConstructor n xs) = PConstructor n (apply s xs)
  apply _ (PLiteral l) = PLiteral l
  apply s (PSpecialVar n t) = PSpecialVar n (apply s t)
  apply _ PWildcard = PWildcard
  apply s (PList xs sl) = PList (apply s xs) (apply s sl)

instance Free TypeError where
  free _ = mempty
  apply _ (CompilerError e) = CompilerError e
  apply s (NoExtensionFound n t) = NoExtensionFound n (apply s t)
  apply s (MultipleExtensionsFound n ts t) = MultipleExtensionsFound n (apply s ts) (apply s t)
  apply _ e = e