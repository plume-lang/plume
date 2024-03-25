module Main where

import Control.Monad.Exception
import Data.Text.IO
import Plume.Compiler.Bytecode.Assembler
import Plume.Compiler.Bytecode.Serialize
import Plume.Compiler.ClosureConversion.Conversion
import Plume.Compiler.Desugaring.Desugar
import Plume.Compiler.SSA
import Plume.Compiler.TypeErasure.EraseType
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
          let erased = eraseType closed
          desugared <- desugar erased
          let ssa = runSSA desugared
          bytecode <- assembleBytecode ssa
          sbc <- serialize bytecode
          writeFileLBS "example/closure.plm.bc" sbc
