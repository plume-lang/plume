module Plume.Syntax.Translation.ConcreteToAbstract.Operations where

import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Concrete qualified as CST
import Plume.Syntax.Concrete.Internal.Show ()
import Plume.Syntax.Translation.Generics

convertOperation :: Translator CST.Expression AST.Expression
convertOperation f (CST.EBinary op e1 e2) = do
  e1' <- f e1
  e2' <- f e2
  pure $ AST.EApplication (AST.EVariable (toText op)) [e1', e2']
convertOperation f (CST.EPrefix op e) = do
  e' <- f e
  pure $ AST.EApplication (AST.EVariable (toText op)) [e']
convertOperation _ _ = error "Impossible happened"