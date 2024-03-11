module Plume.Syntax.Translation.ConcreteToAbstract.UFCS where

import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Concrete qualified as CST
import Plume.Syntax.Concrete.Internal.Show ()
import Plume.Syntax.Translation.Generics (
  Error (..),
  Translator,
  flat,
  shouldBeAlone,
  transRet,
 )

convertUFCS :: Translator Error CST.Expression AST.Expression
-- A binary expression is just a function application with the operator as
-- the function and the operands as the arguments.
-- So, e1 `op` e2 is equivalent to op(e1, e2).
convertUFCS f (CST.EApplication (CST.EProperty field e1) args) = do
  e1' <- shouldBeAlone <$> f e1
  convertedArgs <- fmap flat . sequence <$> mapM f args
  let newArgs = (:) <$> e1' <*> convertedArgs
  transRet $ AST.EApplication (AST.EVariable field) <$> newArgs
convertUFCS _ _ = return $ Left (CompilerError "Received invalid operation expression")
