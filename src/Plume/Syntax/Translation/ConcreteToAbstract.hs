module Plume.Syntax.Translation.ConcreteToAbstract where

import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Concrete qualified as CST

import Plume.Syntax.Translation.ConcreteToAbstract.Operations
import Plume.Syntax.Translation.ConcreteToAbstract.Require
import Plume.Syntax.Translation.Generics
import System.Directory

concreteToAbstract
  :: CST.Expression
  -> TranslatorReader Text AST.Expression
concreteToAbstract (CST.EVariable n) = transRet . Right $ AST.EVariable n
concreteToAbstract (CST.ELiteral l) = transRet . Right $ AST.ELiteral l
concreteToAbstract e@(CST.EBinary {}) = convertOperation concreteToAbstract e
concreteToAbstract e@(CST.EPrefix {}) = convertOperation concreteToAbstract e
concreteToAbstract (CST.EApplication e es) = do
  -- Checking if the first expression is alone, meaning that it is not
  -- composed of multiple elements (like a spread element), and unwraping
  -- Spreadable if it is.
  e' <- shouldBeAlone <$> concreteToAbstract e
  es' <- fmap flat . sequence <$> mapM concreteToAbstract es
  transRet $ AST.EApplication <$> e' <*> es'
concreteToAbstract (CST.EDeclaration ann e me) = do
  -- Declaration and body value cannot be spread elements, so we need to
  -- check if they are alone and unwrap them if they are.
  e' <- shouldBeAlone <$> concreteToAbstract e
  me' <- mapM shouldBeAlone <$> maybeM concreteToAbstract me
  transRet $ AST.EDeclaration ann <$> e' <*> me'
concreteToAbstract (CST.EConditionBranch e1 e2 e3) = do
  -- A condition should be a single expression
  e1' <- shouldBeAlone <$> concreteToAbstract e1

  -- But the branches can be spread elements, so we need to check if they
  -- are, and then combine them into a single expression by wrapping them
  -- into a block.
  e2' <- fmap (AST.EBlock . fromSpreadable) <$> concreteToAbstract e2
  e3' <- fmap (AST.EBlock . fromSpreadable) <$> concreteToAbstract e3
  transRet $ AST.EConditionBranch <$> e1' <*> e2' <*> e3'
concreteToAbstract (CST.EClosure anns t e) = do
  -- Same method as described for condition branches
  e' <- fmap (AST.EBlock . fromSpreadable) <$> concreteToAbstract e
  transRet $ AST.EClosure anns t <$> e'
concreteToAbstract (CST.EBlock es) = do
  -- Blocks can be composed of spread elements, so we need to flatten
  -- the list of expressions into a single expression.
  es' <- fmap flat . sequence <$> mapM concreteToAbstract es
  transRet $ AST.EBlock <$> es'
concreteToAbstract CST.ERowEmpty = bireturn $ Single AST.ERowEmpty
concreteToAbstract (CST.ERowExtension l e1 e2) = do
  -- We can't represent spread elements in row extensions
  e1' <- shouldBeAlone <$> concreteToAbstract e1
  e2' <- shouldBeAlone <$> concreteToAbstract e2
  transRet $ AST.ERowExtension l <$> e1' <*> e2'
concreteToAbstract (CST.ERowSelect e l) = do
  -- Same for row selections as we select on a single record
  e' <- shouldBeAlone <$> concreteToAbstract e
  transRet $ AST.ERowSelect <$> e' <*> pure l
concreteToAbstract (CST.ERowRestrict e l) = do
  -- Row restrict don't work too on spread elements
  e' <- shouldBeAlone <$> concreteToAbstract e
  transRet $ AST.ERowRestrict <$> e' <*> pure l
concreteToAbstract r@(CST.ERequire _) =
  convertRequire concreteToAbstract r
concreteToAbstract (CST.ELocated e p) = do
  e' <- concreteToAbstract e
  return $ case e' of
    -- Located expressions can consist of regular expressions, wrapped
    -- in Single constructor
    Right (Single e'') -> return (Single (AST.ELocated e'' p))
    -- But if they're spread elements, we need to get rid of the location
    -- information as this is misleading
    Right (Spread es) -> return (Spread es)
    -- If there's an error, we just return it
    Left err -> Left err

runConcreteToAbstract :: [CST.Expression] -> IO (Either Text [AST.Expression])
runConcreteToAbstract x = do
  -- Getting the current working directory as a starting point
  -- for the reader monad
  cwd <- getCurrentDirectory

  runReaderT (fmap flat . sequence <$> mapM concreteToAbstract x) cwd
