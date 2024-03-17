module Main where

import Control.Monad.Exception
import Data.Text.IO
import Plume.Compiler.ClosureConversion.Conversion
import Plume.Syntax.Parser
import Plume.Syntax.Translation.ConcreteToAbstract
import Plume.TypeChecker.Checker
import Plume.TypeChecker.TLIR.Internal.Pretty ()
import Prelude hiding (readFile)

main :: IO ()
main = do
  let file = "example/closure.plm"
  content <- readFile file

  parsePlumeFile file content `with` \cst -> do
    runConcreteToAbstract cst `with` \ast -> do
      runSynthesize ast `with` \tlir -> do
        runClosureConversion tlir `with` \closed -> do
          mapM_ print closed
