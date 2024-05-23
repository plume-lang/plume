module Plume.TypeChecker.Monad.Free where

import Plume.Syntax.Common.Annotation
import Plume.TypeChecker.TLIR
import Data.Set qualified as Set

substituteVar :: Expression -> (Text, Text) -> Expression
substituteVar (EVariable n1 t) (n2, repl)
  | n1 == n2 = EVariable repl t
substituteVar (EApplication x xs) r = EApplication (substituteVar x r) (map (`substituteVar` r) xs)
substituteVar (EExtVariable n1 t t') (n2, repl)
  | n1 == n2 = EExtVariable repl t t'
substituteVar (EInstanceVariable n1 t) (n2, repl)
  | n1 == n2 = EInstanceVariable repl t
substituteVar (EList xs) r = EList $ map (`substituteVar` r) xs
substituteVar (EEqualsType e t) r = EEqualsType (substituteVar e r) t
substituteVar (EAnd e1 e2) r = EAnd (substituteVar e1 r) (substituteVar e2 r)
substituteVar (EIndex e i) r = EIndex (substituteVar e r) (substituteVar i r)
substituteVar (EInstanceAccess e i) r = EInstanceAccess (substituteVar e r) i
substituteVar (EInstanceDict n t exprs) r = EInstanceDict n t (map (`substituteVar` r) exprs)
substituteVar (EDeclaration ann e1 e2) r@(n, _)
  | ann.annotationName /= n = EDeclaration ann (substituteVar e1 r) (substituteVar <$> e2 <*> pure r)
substituteVar (EMutDeclaration ann e1 e2) r@(n, _)
  | ann.annotationName /= n = EMutDeclaration ann (substituteVar e1 r) (substituteVar <$> e2 <*> pure r)
substituteVar (EMutUpdate ann e1 e2) r@(n, _)
  | ann.annotationName /= n = EMutUpdate ann (substituteVar e1 r) (substituteVar <$> e2 <*> pure r)
substituteVar (EUnMut e) r = EUnMut $ substituteVar e r
substituteVar (EConditionBranch c t e) r = EConditionBranch (substituteVar c r) (substituteVar t r) (substituteVar <$> e <*> pure r)
substituteVar (EClosure anns ret body p) r@(n, _)
  | n `notElem` names = EClosure anns ret (substituteVar body r) p
  where names = map (.annotationName) anns
substituteVar (EBlock es) r = EBlock (map (`substituteVar` r) es)
substituteVar (ESwitch e cases) r = ESwitch (substituteVar e r) (map (`substitutePat` r) cases)
substituteVar (EReturn e) r = EReturn (substituteVar e r)
substituteVar (ESpreadable es) r = ESpreadable (map (`substituteVar` r) es)
substituteVar x _ = x

freePat :: Pattern -> Set Text
freePat (PVariable n _) = Set.singleton n
freePat (PConstructor n _ ps) = Set.singleton n <> foldMap freePat ps
freePat (PSpecialVar n _) = Set.singleton n
freePat (PList _ ps sl) = foldMap freePat ps <> maybe mempty freePat sl
freePat _ = mempty

substitutePat :: (Pattern, Expression) -> (Text, Text) -> (Pattern, Expression)
substitutePat (pat, e) r@(n, _) 
  | n `Set.notMember` freePat pat = (pat, substituteVar e r)
substitutePat e _ = e