module Main (main) where

import Test.Hspec

import Internal.Parser qualified as Parser

main :: IO ()
main = do

  hspec $ do
    Parser.testLiteral
    Parser.testParser
