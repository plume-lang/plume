module Main where

import Data.Text.IO
import Language.Feather.Parser
import Text.Megaparsec (errorBundlePretty)
import Prelude hiding (print, putStrLn, readFile)

printText :: (ToText a) => a -> IO ()
printText = putStrLn . toText

main :: IO ()
main = do
  content <- readFile "example/main.fth"
  let x = parseTest content
  case x of
    Left e -> putStrLn . toText $ errorBundlePretty e
    Right r -> mapM_ printText r
