module Main where

import Data.Text.IO
import Plume.Syntax.Concrete.Internal.Pretty ()
import Plume.Syntax.Parser
import System.IO.Pretty
import Text.Megaparsec (errorBundlePretty)
import Prelude hiding (readFile)

main :: IO ()
main = do
  content <- readFile "example/main.plm"
  let x = parseTest content
  case x of
    Left e -> printText $ errorBundlePretty e
    Right r -> ppPrint r
