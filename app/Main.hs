{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Exception
import Control.Monad.Parser
import Data.Text.IO hiding (putStr)
import Plume.Compiler.ClosureConversion.Conversion
import Plume.Compiler.Desugaring.Desugar
import Plume.Compiler.TypeErasure.EraseType
import Plume.Syntax.Parser.Modules.ParseImports
import Plume.Syntax.Require.Resolution
import Plume.Syntax.Translation.ConcreteToAbstract
import Plume.Compiler.Bytecode.Syntax (Instruction)
import Plume.Compiler.Javascript.Translate
import Plume.Compiler.LLIR.Assembler
import Plume.Compiler.Bytecode.Assembler
import Plume.Compiler.Bytecode.Serialize
-- import Plume.Compiler.Bytecode.Syntax (Instruction)
import Plume.Compiler.Bytecode.Label hiding (labelPool)
import Plume.TypeChecker.Checker
-- import Plume.Syntax.Memory
import Plume.Syntax.Blocks
import System.Directory
import System.FilePath
import System.IO.Pretty
import Prelude hiding (putStrLn, readFile)
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

main :: IO ()
main = setEncoding $ do
  args <- getArgs
  let ext_type = fromString . fromMaybe "native" $ maybeAt 0 args
  let file_input = maybeAt 1 args
  env <- lookupEnv "PLUME_PATH"
  mod' <- lookupEnv "PPM_PATH"

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
      writeIORef extensionType ext_type

      void $ checkModule (env, mod') file

      runConcreteToAbstract env dir paths' file `with` \ast -> do
        let ast' = concatMap (removeUselessBlocks (False, False)) ast
        ppBuilding "Typechecking..."
        runSynthesize ast' `with` \tlir -> do
          ppBuilding "Compiling and optimizing..."
          erased <- erase tlir
          runClosureConversion erased `with` \closed -> do
            desugared <- desugar closed

            case ext_type of
              "js" -> do
                let js   = runTranslateJS desugared
                    code = createMainJSApp js

                writeFileText (replaceExtension file ".js") code
                ppSuccess ("Bytecode written to " <> fromString (replaceExtension file ".js"))

              _ -> do
                (bytecode, natives', constants) <- runLLIRAssembler desugared
                let nativeFuns = getNativeFunctions natives'
                
                globals' <- readIORef globals

                (bytecode', labelPool) <- runUnlabelize bytecode

                finalBytecode <- runBytecodeAssembler (labelPool, nativeFuns) bytecode'

                sbc <- serialize (finalBytecode, natives', constants)
                let new_path = file -<.> "bin"
                writeFileLBS new_path sbc
                ppSuccess ("Bytecode written to " <> fromString new_path)
    Nothing -> ppFailure "No input file provided"

printBytecode :: [Instruction] -> IO ()
printBytecode bytecode =
  mapM_
    ( \(i, instr) -> do
        putStr (show i <> ": ")
        print instr
    )
    (zip [0 :: Int ..] bytecode)