module Plume.Syntax.Translation.ConcreteToAbstract where

import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Concrete qualified as CST

import Plume.Syntax.Translation.ConcreteToAbstract.Operations
import Plume.Syntax.Translation.ConcreteToAbstract.Require

maybeM :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f = maybe (return Nothing) (fmap Just . f)

bireturn :: (Monad m1, Monad m2) => a -> m1 (m2 a)
bireturn = return . return

concreteToAbstract :: CST.Expression -> IO (Either Text AST.Expression)
concreteToAbstract (CST.EVariable n) = bireturn $ AST.EVariable n
concreteToAbstract (CST.ELiteral l) = bireturn $ AST.ELiteral l
concreteToAbstract e@(CST.EBinary{}) = convertOperation concreteToAbstract e
concreteToAbstract e@(CST.EPrefix{}) = convertOperation concreteToAbstract e
concreteToAbstract (CST.EApplication e es) = do
  e' <- concreteToAbstract e
  es' <- sequence <$> mapM concreteToAbstract es
  return $ AST.EApplication <$> e' <*> es'
concreteToAbstract (CST.EDeclaration ann e me) = do
  e' <- concreteToAbstract e
  me' <- sequence <$> maybeM concreteToAbstract me
  return $ AST.EDeclaration ann <$> e' <*> me'
concreteToAbstract (CST.EConditionBranch e1 e2 e3) = do
  e1' <- concreteToAbstract e1
  e2' <- concreteToAbstract e2
  e3' <- concreteToAbstract e3
  return $ AST.EConditionBranch <$> e1' <*> e2' <*> e3'
concreteToAbstract (CST.EClosure anns t e) = do
  e' <- concreteToAbstract e
  return $ AST.EClosure anns t <$> e'
concreteToAbstract (CST.EBlock es) = do
  es' <- sequence <$> mapM concreteToAbstract es
  return $ AST.EBlock <$> es'
concreteToAbstract CST.ERowEmpty = bireturn AST.ERowEmpty
concreteToAbstract (CST.ERowExtension l e1 e2) = do
  e1' <- concreteToAbstract e1
  e2' <- concreteToAbstract e2
  return $ AST.ERowExtension l <$> e1' <*> e2'
concreteToAbstract (CST.ERowSelect e l) = do
  e' <- concreteToAbstract e
  return $ AST.ERowSelect <$> e' <*> pure l
concreteToAbstract (CST.ERowRestrict e l) = do
  e' <- concreteToAbstract e
  return $ AST.ERowRestrict <$> e' <*> pure l
concreteToAbstract r@(CST.ERequire _) = convertRequire concreteToAbstract r
concreteToAbstract (CST.ELocated e p) = do
  e' <- concreteToAbstract e
  return $ AST.ELocated <$> e' <*> pure p
