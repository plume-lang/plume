module Plume.Syntax.Translation.ConcreteToAbstract.Require where

import Control.Monad.Exception
import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Concrete qualified as CST
import Plume.Syntax.Parser
import Plume.Syntax.Translation.Generics
import System.FilePath

convertRequire
  :: Translator Text CST.Expression AST.Expression
convertRequire f (CST.ERequire modName) = do
  -- Retrieve the current working directory to use it as a base for the
  -- module path.
  cwd <- ask

  -- Creating the absolute module path by joining the current working
  -- directory with the module name.
  let strModName = toString modName
  let modPath = cwd </> strModName

  -- Reading the content of the module file.
  content <- decodeUtf8 @Text <$> readFileBS strModName

  -- Creating the next module current directory by taking the directory
  -- part of the module path.
  let newCurrentDirectory = takeDirectory modPath

  -- Parsing the module file and converting it to an abstract syntax tree.
  -- We need to use the local function to change the current directory
  -- without globally changing it.
  -- Returning the generated AST as a spreadable AST (just a list of
  -- expressions represented as a single expression).
  local (const newCurrentDirectory) $
    liftIO (parsePlumeFile modPath content) `with` \cst ->
      sequence <$> mapM f cst `with` \ast -> do
        bireturn . Spread $ flat ast
convertRequire _ _ = error "Impossible happened"
