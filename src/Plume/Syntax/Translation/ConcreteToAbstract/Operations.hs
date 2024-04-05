module Plume.Syntax.Translation.ConcreteToAbstract.Operations where

import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Concrete qualified as CST
import Plume.Syntax.Concrete.Internal.Show ()
import Plume.Syntax.Translation.Generics (
  Error (..),
  Translator,
  shouldBeAlone,
  transRet,
 )

convertOperation :: Translator Error CST.Expression AST.Expression
-- A binary expression is just a function application with the operator as
-- the function and the operands as the arguments.
-- So, e1 `op` e2 is equivalent to op(e1, e2).
convertOperation f (CST.EBinary op e1 e2) = do
  e1' <- shouldBeAlone <$> f e1
  e2' <- shouldBeAlone <$> f e2
  let args = sequence [e1', e2']
  transRet $ AST.EApplication (AST.EVariable (toText op)) <$> args
-- A prefix expression is just a function application with the operator as
-- the function and the operand as the argument.
-- So, op e is equivalent to op(e).
convertOperation f (CST.EPrefix op e) = do
  e' <- fmap (: []) . shouldBeAlone <$> f e
  transRet $ AST.EApplication (AST.EVariable (toText op)) <$> e'
convertOperation f (CST.EPostfix op e) = do
  e' <- fmap (: []) . shouldBeAlone <$> f e
  transRet $ AST.EApplication (AST.EVariable (toText op)) <$> e'
convertOperation _ _ = return $ Left (CompilerError "Received invalid operation expression")
