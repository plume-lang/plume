module Plume.Syntax.Translation.ConcreteToAbstract.UFCS where

import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Concrete qualified as CST
import Plume.Syntax.Concrete.Internal.Show ()
import Plume.Syntax.Translation.Generics 

-- | UFCS stands for Universal Function Call Syntax.
-- | UFCS Conversion consists in removing dot calls and converting
-- | them to basic function calls. 
-- | For instance `a.b(c)` will be converted to `b(a, c)`.
-- | It can handle dot chained calls too
convertUFCS :: Translator Error CST.Expression AST.Expression
convertUFCS f (CST.EApplication (CST.EProperty field e1) args) = do
  e1' <- shouldBeAlone <$> f e1
  convertedArgs <- fmap flat . sequence <$> mapM f args
  let newArgs = (:) <$> e1' <*> convertedArgs
  transRet $ AST.EApplication (AST.EVariable field) <$> newArgs
convertUFCS _ _ = return $ Left (CompilerError "Received invalid operation expression")
