module Main where

import Control.Monad.Exception
import Data.Text.IO
import Plume.Syntax.Parser
import Plume.Syntax.Translation.ConcreteToAbstract
import Plume.TypeChecker.Checker hiding (with)
import Plume.TypeChecker.TLIR.Internal.Pretty ()
import System.IO.Pretty
import Prelude hiding (readFile)

main :: IO ()
main = do
  let file = "example/typecheck.plm"
  content <- readFile file

  parsePlumeFile file content `with` \cst -> do
    runConcreteToAbstract cst `with` \ast -> do
      runSynthesize ast `with` \tlir -> do
        ppPrint tlir
