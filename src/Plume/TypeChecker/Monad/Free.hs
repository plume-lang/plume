module Plume.TypeChecker.Monad.Free where

import Plume.Syntax.Common.Annotation
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR
import Plume.TypeChecker.Constraints.Unification (compressPaths)
import Plume.Syntax.Concrete (TypeConstructor(..))

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
freePattern (PConstructor name xs) = PConstructor name <$> mapM freePattern xs
freePattern (PSpecialVar name t) = PSpecialVar name <$> compressPaths t
freePattern (PList xs sl) = PList <$> mapM freePattern xs <*> traverse freePattern sl
freePattern (PLiteral l) = pure $ PLiteral l
freePattern PWildcard = pure PWildcard

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
free (ENativeFunction name t1 t2) = do
  t2' <- compressPaths t2
  pure $ ENativeFunction name t1 t2'
free (ELiteral l) = pure $ ELiteral l