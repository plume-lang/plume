{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Monad.Exception
import Control.Monad.Parser
import Data.Either
import Data.Text.IO hiding (putStr)
import Plume.Compiler.Bytecode.Assembler
import Plume.Compiler.Bytecode.Serialize
import Plume.Compiler.Bytecode.Syntax
import Plume.Compiler.ClosureConversion.Conversion
import Plume.Compiler.Desugaring.Desugar
import Plume.Compiler.SSA
import Plume.Compiler.TypeErasure.EraseType
import Plume.Syntax.Abstract.Internal.Pretty ()
import Plume.Syntax.Parser.Modules.ParseImports
import Plume.Syntax.Translation.ConcreteToAbstract
import Plume.TypeChecker.Checker
import System.Directory
import System.FilePath
import Prelude hiding (putStrLn, readFile)
import System.IO.Pretty
#if defined(mingw32_HOST_OS)
import System.IO (hPutStrLn, hSetEncoding, stdout, utf8)
import System.Win32.Console (getConsoleOutputCP, setConsoleOutputCP)

setEncoding :: IO a -> IO a
setEncoding a = do
  cp <- getConsoleOutputCP
  setConsoleOutputCP 65001
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetEncoding stdin utf8

  res <- a

  setConsoleOutputCP cp

  pure res
  
#else
setEncoding :: IO a -> IO a
setEncoding = id
#endif

fromEither :: a -> Either b a -> a
fromEither _ (Right a) = a
fromEither a _ = a


main :: IO ()
main = setEncoding $ do
  file_input <- maybeAt 0 <$> getArgs
  env <- lookupEnv "PLUME_PATH"

  case file_input of
    Just file -> do
      doesFileExist file >>= \case
        False -> do
          ppFailure ("File " <> fromString file <> " does not exist")
          exitFailure
        True -> pure ()

      cwd <- getCurrentDirectory >>= canonicalizePath
      let dir = cwd </> takeDirectory file

      content <- readFile file

      paths <- fromEither [] <$> parse getPaths file content
      let paths' = case env of
            Just _ -> ("std:prelude", Nothing) : paths
            Nothing -> paths

      ppBuilding "Parsing file and dependencies..."
      runConcreteToAbstract env dir paths' file `with` \ast -> do
        ppBuilding "Typechecking..."
        runSynthesize ast `with` \tlir -> do
          ppBuilding "Compiling and optimizing..."
          erased <- erase tlir
          runClosureConversion erased `with` \closed -> do
            desugared <- desugar closed
            let ssa = runSSA desugared
            bytecode <- assembleBytecode dir ssa
            sbc <- serialize bytecode
            let new_path = file -<.> "bin"
            writeFileLBS new_path sbc
            ppSuccess ("Bytecode written to " <> fromString new_path)
    Nothing -> ppFailure "No input file provided"

printBytecode :: Program -> IO ()
printBytecode bytecode =
  mapM_
    ( \(i, instr) -> do
        putStr (show i <> ": ")
        print instr
    )
    (zip [0 :: Int ..] $ pInstructions bytecode)
