module Main where

import Language.Feather.Parser
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  content <- readFile "example/main.fth"
  let x = parseTest content
  case x of
    Left e -> putStrLn $ errorBundlePretty e
    Right r -> mapM_ print r
