{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Exception
import Data.Text.IO hiding (putStr)
import Plume.Compiler.Bytecode.Assembler hiding (nativeLibraries)
import Plume.Compiler.Bytecode.Serialize

import Plume.Compiler.Bytecode.Syntax
import Plume.Compiler.ClosureConversion.Conversion
import Plume.Compiler.Desugaring.Desugar
import Plume.Compiler.SSA
import Plume.Compiler.TypeErasure.EraseType
import Plume.Syntax.Parser
import Plume.Syntax.Translation.ConcreteToAbstract
import Plume.TypeChecker.Checker
import System.Directory
import System.FilePath
import Prelude hiding (putStrLn, readFile)

main :: IO ()
main = do
  file_input <- maybeAt 0 <$> getArgs
  env <- lookupEnv "PLUME_PATH"

  case file_input of
    Just file -> do
      doesFileExist file >>= \case
        False -> do
          putStr "File "
          print file
          putStrLn " does not exist"
          exitFailure
        True -> pure ()

      cwd <- getCurrentDirectory >>= canonicalizePath
      let dir = cwd </> takeDirectory file

      content <- readFile file

      parsePlumeFile file content `with` \cst -> do
        runConcreteToAbstract env dir cst `with` \ast -> do
          runSynthesize ast `with` \tlir -> do
            erased <- erase tlir
            runClosureConversion erased `with` \closed -> do
              desugared <- desugar closed
              let ssa = runSSA desugared
              bytecode <- assembleBytecode dir ssa
              sbc <- serialize bytecode
              let new_path = file -<.> "bin"
              writeFileLBS new_path sbc
              putStrLn $ "Bytecode written to " <> fromString new_path
    Nothing -> putStrLn "No file provided"

printBytecode :: Program -> IO ()
printBytecode bytecode =
  mapM_
    ( \(i, instr) -> do
        putStr (show i <> ": ")
        print instr
    )
    (zip [0 :: Int ..] $ instructions bytecode)