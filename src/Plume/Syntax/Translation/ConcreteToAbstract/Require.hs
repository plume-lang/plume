module Plume.Syntax.Translation.ConcreteToAbstract.Require where

import GHC.IO
import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Concrete qualified as CST
import Plume.Syntax.Translation.Generics
import System.Directory.OsPath
import System.OsPath

{-# NOINLINE currentDirectory #-}
currentDirectory :: IORef OsPath
currentDirectory = unsafePerformIO $ newIORef =<< getCurrentDirectory

convertRequire :: Translator Text CST.Expression AST.Expression
convertRequire _ (CST.ERequire modName) = do
  print modName
  return $ return (AST.EVariable modName)
convertRequire _ _ = error "Impossible happened"
