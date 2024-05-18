module Plume.TypeChecker.Monad.Free where

import Plume.Syntax.Common.Annotation
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR
import Plume.TypeChecker.Constraints.Unification (compressPaths)
import Plume.Syntax.Concrete (TypeConstructor(..))
import Data.Set qualified as Set

freeAnn :: Annotation PlumeType -> IO (Annotation PlumeType)
freeAnn (Annotation name t) = do
  t' <- compressPaths t
  pure $ Annotation name t'

freeTypeConstructor :: TypeConstructor PlumeType -> IO (TypeConstructor PlumeType)
freeTypeConstructor (TVariable name) = pure $ TVariable name
freeTypeConstructor (TConstructor name xs) = do
  xs' <- mapM compressPaths xs
  pure $ TConstructor name xs'

freePattern :: Pattern -> IO Pattern
freePattern (PVariable name t) = PVariable name <$> compressPaths t
freePattern (PConstructor name t xs) = PConstructor name <$> compressPaths t <*> mapM freePattern xs
freePattern (PSpecialVar name t) = PSpecialVar name <$> compressPaths t
freePattern (PList t xs sl) = PList <$> compressPaths t <*> mapM freePattern xs <*> traverse freePattern sl
freePattern (PLiteral l) = pure $ PLiteral l
freePattern (PWildcard t) = PWildcard <$> compressPaths t

free :: Expression -> IO Expression
free (EVariable name t) = do
  t' <- compressPaths t
  pure $ EVariable name t'
free (EApplication f xs) = do
  f' <- free f
  xs' <- mapM free xs
  pure $ EApplication f' xs'
free (EExtVariable name t1 t2) = do
  t1' <- compressPaths t1
  t2' <- compressPaths t2
  pure $ EExtVariable name t1' t2'
free (EList xs) = EList <$> mapM free xs
free (EEqualsType e t) = EEqualsType <$> free e <*> pure t
free (EAnd e1 e2) = EAnd <$> free e1 <*> free e2
free (EIndex e1 e2) = EIndex <$> free e1 <*> free e2
free (EType t xs) = EType t <$> mapM freeTypeConstructor xs
free (EDeclaration ann e1 e2) = do
  ann' <- freeAnn ann
  e1' <- free e1
  e2' <- traverse free e2
  pure $ EDeclaration ann' e1' e2'
free (EMutDeclaration ann e1 e2) = do
  ann' <- freeAnn ann
  e1' <- free e1
  e2' <- traverse free e2
  pure $ EMutDeclaration ann' e1' e2'
free (EMutUpdate ann e1 e2) = do
  ann' <- freeAnn ann
  e1' <- free e1
  e2' <- traverse free e2
  pure $ EMutUpdate ann' e1' e2'
free (EUnMut e) = EUnMut <$> free e
free (EExtensionDeclaration name t1 ann e) = do
  ann' <- freeAnn ann
  e' <- free e
  pure $ EExtensionDeclaration name t1 ann' e'
free (EConditionBranch e1 e2 e3) = do
  e1' <- free e1
  e2' <- free e2
  e3' <- traverse free e3
  pure $ EConditionBranch e1' e2' e3'
free (EClosure anns t e) = do
  anns' <- mapM freeAnn anns
  e' <- free e
  pure $ EClosure anns' t e'
free (EBlock xs) = EBlock <$> mapM free xs
free (ELocated e p) = ELocated <$> free e <*> pure p
free (ESwitch e xs) = do
  e' <- free e
  xs' <- mapM (\(p, b) -> (,) <$> freePattern p <*> free b) xs
  pure $ ESwitch e' xs'
free (EReturn e) = EReturn <$> free e
free (ENativeFunction name t1 t2 st) = do
  t2' <- compressPaths t2
  pure $ ENativeFunction name t1 t2' st
free (ELiteral l) = pure $ ELiteral l
free (EInstanceAccess e i) = EInstanceAccess <$> free e <*> pure i
free (EInstanceDict n t exprs) = EInstanceDict n t <$> mapM free exprs
free (ESpreadable es) = ESpreadable <$> mapM free es
free (EInstanceVariable n t) = EInstanceVariable n <$> compressPaths t
free EEmpty = pure EEmpty

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
substituteVar (EClosure anns ret body) r@(n, _)
  | n `notElem` names = EClosure anns ret (substituteVar body r)
  where names = map (.annotationName) anns
substituteVar (EBlock es) r = EBlock (map (`substituteVar` r) es)
substituteVar (ELocated e p) r = ELocated (substituteVar e r) p
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