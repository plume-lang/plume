module Main where

import Control.Monad.Exception
import Data.Text.IO hiding (putStr)
import Plume.Compiler.Bytecode.Assembler
import Plume.Compiler.Bytecode.Serialize

-- import Plume.Compiler.Bytecode.Syntax
import Plume.Compiler.ClosureConversion.Conversion
import Plume.Compiler.Desugaring.Desugar
import Plume.Compiler.SSA
import Plume.Compiler.TypeErasure.EraseType
import Plume.Syntax.Parser
import Plume.Syntax.Translation.ConcreteToAbstract
import Plume.TypeChecker.Checker
import Plume.TypeChecker.TLIR.Internal.Pretty ()

-- import System.IO.Pretty
import Prelude hiding (readFile)

main :: IO ()
main = do
  let file = "example/closure.plm"
  content <- readFile file

  parsePlumeFile file content `with` \cst -> do
    runConcreteToAbstract cst `with` \ast -> do
      runSynthesize ast `with` \tlir -> do
        let erased = eraseType tlir
        runClosureConversion erased `with` \closed -> do
          desugared <- desugar closed
          let ssa = runSSA desugared
          -- mapM_ print ssa
          bytecode <- assembleBytecode ssa
          sbc <- serialize bytecode
          -- mapM_
          --   ( \(i, instr) -> do
          --       putStr (show i <> ": ")
          --       print instr
          --   )
          --   (zip [0 ..] $ instructions bytecode)
          writeFileLBS "example/closure.plm.bc" sbc
