module Main where

import Control.Monad.Exception
import Data.Text.IO
import Plume.Syntax.Abstract.Internal.Pretty ()
import Plume.Syntax.Parser
import Plume.Syntax.Translation.ConcreteToAbstract
import System.IO.Pretty
import Prelude hiding (readFile)

main :: IO ()
main = do
  let file = "example/main.plm"
  content <- readFile file

  parsePlumeFile file content `with` \cst -> do
    sequence <$> mapM concreteToAbstract cst `with` \ast -> do
      ppPrint ast
