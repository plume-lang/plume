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

  catchIO (parsePlumeFile file content) $ \cst -> do
    catchIO (sequence <$> mapM concreteToAbstract cst) $ \ast -> do
      ppPrint ast
