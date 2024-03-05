module Main where

import Data.Text.IO
import Language.Feather.Parser
import Print
import Text.Megaparsec (errorBundlePretty)
import Prelude hiding (readFile)

main :: IO ()
main = do
  content <- readFile "example/main.fth"
  let x = parseTest content
  case x of
    Left e -> printText $ errorBundlePretty e
    Right r -> ppPrint r
