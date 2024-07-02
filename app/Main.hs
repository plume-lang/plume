module Main where

import Language.Plume.Frontend.Parser qualified as P
import Language.Plume.Frontend.Parser.Declaration qualified as P
import Language.Plume.Frontend.Module.Resolution qualified as M
import Language.Plume.Frontend.TypeChecking qualified as Tc
import Language.Plume.Backend.TLIR qualified as TLIR
import Language.Plume.Backend.Closure.Conversion qualified as CC
import Language.Plume.Backend.Monomorphization.Conversion qualified as Mono
import Language.Plume.Backend.LLIR.Conversion qualified as LLIR
import Language.Plume.Backend.CLang.Codegen qualified as CLang

import Control.Monad.Result
import qualified Language.Plume.Backend.ANF.Conversion as ANF
import qualified Language.Plume.Backend.CFG.Conversion as CFG

main :: IO ()
main = do
  let filePath = "example/file.plm"
  fileContent <- readFileBS filePath
  let fileContentAsText :: Text = decodeUtf8 fileContent

  result <- P.parsePlumeFile filePath fileContentAsText P.parseProgram

  case result of
    Left err -> putStrLn (showError err)
    Right ast -> do
      ms <- M.createModuleState filePath
      ast' <- M.resolveModules ast ms

      handle ast' $ \astWithoutRequires -> do
        typedAST <- Tc.typecheck astWithoutRequires

        handle typedAST $ \typedAST' -> do
          let mlir = map TLIR.declToMLIR typedAST'

          cc <- CC.runClosureConversion mlir
          mono <- Mono.monomorphize cc
          anf <- concat <$> mapM ANF.convert mono

          -- mapM_ print anf

          llir <- LLIR.convertToLLIR anf
          let cfg = map CFG.convert llir
          ir <- CLang.runCLang cfg

          writeFile "output.c" (intercalate "\n" (map show ir))

          -- llvm <- LLVM.runLLVMPass llir

          -- writeFile "output.ll" (toString llvm)
