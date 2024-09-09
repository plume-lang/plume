module Internal.Parser where

import Test.Hspec
import Plume.Syntax.Parser.Parser
import Control.Monad.Parser
import Plume.Syntax.Parser.Modules.Literal

testLiteral :: Spec
testLiteral = do
  describe "literal parsing" $ do

    it "should parse correctly a string" $ do
      -- Basic string parsing
      r <- parse stringLiteral "" "\"hello\""
      r `shouldSatisfy` isRight

      -- String with escape characters
      r2 <- parse stringLiteral "" "\"hello\\nworld\""
      r2 `shouldSatisfy` isRight

      -- String with escape characters
      r3 <- parse stringLiteral "" "\"hello\\tworld\""
      r3 `shouldSatisfy` isRight

    it "should not parse an invalid string" $ do
      r1 <- parse stringLiteral "" "\"hello"
      r1 `shouldSatisfy` isLeft

      r2 <- parse stringLiteral "" "hello\""
      r2 `shouldSatisfy` isLeft

      r3 <- parse stringLiteral "" "\"hello\\"
      r3 `shouldSatisfy` isLeft
    
    it "should parse correctly a char" $ do
      r <- parse charLiteral "" "'a'"
      r `shouldSatisfy` isRight

      r2 <- parse charLiteral "" "'\\n'"
      r2 `shouldSatisfy` isRight

      r3 <- parse charLiteral "" "'\\t'"
      r3 `shouldSatisfy` isRight
    
    it "should not parse an invalid char" $ do
      r1 <- parse charLiteral "" "'a"
      r1 `shouldSatisfy` isLeft

      r2 <- parse charLiteral "" "a'"
      r2 `shouldSatisfy` isLeft

      r3 <- parse charLiteral "" "'\\"
      r3 `shouldSatisfy` isLeft
    
    it "should parse correctly an integer" $ do
      r <- parse integer "" "123"
      r `shouldSatisfy` isRight

      -- Signed integers
      r2 <- parse integer "" "-123"
      r2 `shouldSatisfy` isRight

      r3 <- parse integer "" "+123"
      r3 `shouldSatisfy` isRight

    it "should not parse an invalid integer" $ do
      r2 <- parse integer "" "a123"
      r2 `shouldSatisfy` isLeft
  
    it "should parse correctly a float" $ do
      r <- parse float "" "123.123"
      r `shouldSatisfy` isRight

      -- Signed floats
      r2 <- parse float "" "-123.123"
      r2 `shouldSatisfy` isRight

      r3 <- parse float "" "+123.123"
      r3 `shouldSatisfy` isRight
    
    it "should not parse an invalid float" $ do
      r2 <- parse float "" "a123.123"
      r2 `shouldSatisfy` isLeft

    it "should parse correctly a boolean" $ do
      r <- parse parseBool "" "true"
      r `shouldSatisfy` isRight

      r2 <- parse parseBool "" "false"
      r2 `shouldSatisfy` isRight
    
    it "should not parse an invalid boolean" $ do
      r2 <- parse parseBool "" "tru"
      r2 `shouldSatisfy` isLeft

      r3 <- parse parseBool "" "fals"
      r3 `shouldSatisfy` isLeft

      r6 <- parse parseBool "" "xtrue"
      r6 `shouldSatisfy` isLeft

      r7 <- parse parseBool "" "xfalse"
      r7 `shouldSatisfy` isLeft

    it "should parse correctly a literal" $ do
      r <- parse parseLiteral "" "\"hello\""
      r `shouldSatisfy` isRight

      r2 <- parse parseLiteral "" "'a'"
      r2 `shouldSatisfy` isRight

      r3 <- parse parseLiteral "" "123"
      r3 `shouldSatisfy` isRight

      r4 <- parse parseLiteral "" "123.123"
      r4 `shouldSatisfy` isRight

      r5 <- parse parseLiteral "" "true"
      r5 `shouldSatisfy` isRight

      r6 <- parse parseLiteral "" "false"
      r6 `shouldSatisfy` isRight

    it "should not parse an invalid literal" $ do
      r <- parse parseLiteral "" "tru"
      r `shouldSatisfy` isLeft

      r2 <- parse parseLiteral "" "fals"
      r2 `shouldSatisfy` isLeft

      r3 <- parse parseLiteral "" "xtrue"
      r3 `shouldSatisfy` isLeft

      r4 <- parse parseLiteral "" "xfalse"
      r4 `shouldSatisfy` isLeft

      r5 <- parse parseLiteral "" "\"hello"
      r5 `shouldSatisfy` isLeft

      r6 <- parse parseLiteral "" "hello\""
      r6 `shouldSatisfy` isLeft

      r7 <- parse parseLiteral "" "'a"
      r7 `shouldSatisfy` isLeft

      r8 <- parse parseLiteral "" "a'"
      r8 `shouldSatisfy` isLeft

      r9 <- parse parseLiteral "" "a123"
      r9 `shouldSatisfy` isLeft

      r10 <- parse parseLiteral "" "a123.123"
      r10 `shouldSatisfy` isLeft

      r11 <- parse parseLiteral "" "tru"
      r11 `shouldSatisfy` isLeft

      r12 <- parse parseLiteral "" "fals"
      r12 `shouldSatisfy` isLeft

      r13 <- parse parseLiteral "" "xtrue"
      r13 `shouldSatisfy` isLeft

      r14 <- parse parseLiteral "" "xfalse"
      r14 `shouldSatisfy` isLeft

    it "should parse correctly a string with interpolation" $ do
      r1 <- parse parseStringWithInterpolation "" "\"hello {world}\""
      r1 `shouldSatisfy` isRight

      r2 <- parse parseStringWithInterpolation "" "\"hello {world}!\""
      r2 `shouldSatisfy` isRight

      r3 <- parse parseStringWithInterpolation "" "\"hello {world}! {hello}\""
      r3 `shouldSatisfy` isRight

      r4 <- parse parseStringWithInterpolation "" "\"hello {world}! {hello} {world}\""
      r4 `shouldSatisfy` isRight

    it "should not parse an invalid string with interpolation" $ do
      r1 <- parse parseStringWithInterpolation "" "\"hello {world}"
      r1 `shouldSatisfy` isLeft

      r2 <- parse parseStringWithInterpolation "" "\"hello {world}! {hello"
      r2 `shouldSatisfy` isLeft

      r3 <- parse parseStringWithInterpolation "" "\"hello {world}! {hello} {world"
      r3 `shouldSatisfy` isLeft



testParser ::  Spec
testParser = do
  describe "expression parser" $ do
    it "should parse correctly variables" $ do
      r1 <- parseTest' eVariable "hello"
      r1 `shouldSatisfy` isRight

      r2 <- parseTest' eVariable "helloWorld"
      r2 `shouldSatisfy` isRight

      r3 <- parseTest' eVariable "hello_world"
      r3 `shouldSatisfy` isRight

      r4 <- parseTest' eVariable "hello_world_"
      r4 `shouldSatisfy` isRight

      r5 <- parseTest' eVariable "hello_world_123"
      r5 `shouldSatisfy` isRight

      r6 <- parseTest' eVariable "hello_world_123_"
      r6 `shouldSatisfy` isRight

      r7 <- parseTest' eVariable "hello_world_123_abc"
      r7 `shouldSatisfy` isRight
      
    it "should not parse an invalid variable" $ do
      r1 <- parseTest' eVariable "123"
      r1 `shouldSatisfy` isLeft

      r2 <- parseTest' eVariable "123hello"
      r2 `shouldSatisfy` isLeft

      r3 <- parseTest' eVariable "123_hello"
      r3 `shouldSatisfy` isLeft

      r4 <- parseTest' eVariable "123_hello_"
      r4 `shouldSatisfy` isLeft

      r5 <- parseTest' eVariable "123_hello_123"
      r5 `shouldSatisfy` isLeft

      r6 <- parseTest' eVariable "123_hello_123_"
      r6 `shouldSatisfy` isLeft

      r7 <- parseTest' eVariable "123_hello_123_abc"
      r7 `shouldSatisfy` isLeft

    it "should parse correctly a condition" $ do
      r1 <- parseTest' eIf "if true then 1 else 2"
      r1 `shouldSatisfy` isRight

      r2 <- parseTest' eIf "if true then 1 else if false then 2 else 3"
      r2 `shouldSatisfy` isRight

      r3 <- parseTest' eIf "if (true) { 1 } else { 2 }"
      r3 `shouldSatisfy` isRight

      r4 <- parseTest' eIf "if (true) { 1 } else if (false) { 2 } else { 3 }"
      r4 `shouldSatisfy` isRight

      r5 <- parseTest' eIf "if true then 8"
      r5 `shouldSatisfy` isRight

      r6 <- parseTest' eIf "if true { 8 }"
      r6 `shouldSatisfy` isRight

    it "should not parse an invalid condition" $ do
      r2 <- parseTest' eIf "if true then 1 else"
      r2 `shouldSatisfy` isLeft

      r4 <- parseTest' eIf "if true then 1 else if false then 2 else"
      r4 `shouldSatisfy` isLeft

      r5 <- parseTest' eIf "if true then 1 else if false then 2 else 3 else 4"
      r5 `shouldSatisfy` isLeft

      r6 <- parseTest' eIf "if true then 1 else if false then 2 else 3 else"
      r6 `shouldSatisfy` isLeft

      r7 <- parseTest' eIf "if true then 1 else if false then 2 else 3 else 4"
      r7 `shouldSatisfy` isLeft

      r8 <- parseTest' eIf "if true then 1 else if false then 2 else 3 else 4 else 5"
      r8 `shouldSatisfy` isLeft
    
    it "should parse correctly a block" $ do
      r1 <- parseTest' eBlock "{ 1 }"
      r1 `shouldSatisfy` isRight

      r2 <- parseTest' eBlock "{ 1 2 3 }"
      r2 `shouldSatisfy` isRight

      r3 <- parseTest' eBlock "{ 1 2 3 }"
      r3 `shouldSatisfy` isRight

      r4 <- parseTest' eBlock "{ 1 2 3 4 }"
      r4 `shouldSatisfy` isRight

      r5 <- parseTest' eBlock "{ 1 2 3 4 }"
      r5 `shouldSatisfy` isRight
  
    it "should not parse an invalid block" $ do
      r1 <- parseTest' eBlock "{ 1"
      r1 `shouldSatisfy` isLeft

      r2 <- parseTest' eBlock "1 }"
      r2 `shouldSatisfy` isLeft

      r3 <- parseTest' eBlock "{ 1 2 3"
      r3 `shouldSatisfy` isLeft

      r4 <- parseTest' eBlock "{ 1 2 3 4"
      r4 `shouldSatisfy` isLeft

      r5 <- parseTest' eBlock "1 2 3 4 }"
      r5 `shouldSatisfy` isLeft
    
    it "should parse correctly a switch expression" $ do
      r1 <- parseTest' eSwitch "switch 1 { case 1 => 1 case 2 => 2 }"
      r1 `shouldSatisfy` isRight

      r2 <- parseTest' eSwitch "switch 1 { case 1 => 1 case 2 => 2 case ? => 3 }"
      r2 `shouldSatisfy` isRight

      r3 <- parseTest' eSwitch "switch 1 { case [x, ..xs] => _ case 2 => 2 case ? => 3 }"
      r3 `shouldSatisfy` isRight

      r4 <- parseTest' eSwitch "switch 1 { case (x, y) => _ case 2 => 2 case ? => 3 }"
      r4 `shouldSatisfy` isRight

      r5 <- parseTest' eSwitch "switch 1 { case Some(x) => 3 case None => 9 }"
      r5 `shouldSatisfy` isRight

      r6 <- parseTest' eSwitch "switch 1 { case Some(x) => 3 case None => 9 case ? => 10 }"
      r6 `shouldSatisfy` isRight

    it "should not parse an invalid switch expression" $ do
      r1 <- parseTest' eSwitch "switch 1 { case 1 => 1 case 2 => 2 case ? => 3"
      r1 `shouldSatisfy` isLeft

      r2 <- parseTest' eSwitch "switch 1 { case 1 => 1 2 => 2 case ? => 3 }"
      r2 `shouldSatisfy` isLeft

      r3 <- parseTest' eSwitch "switch 1 { case 1 => 1 case 2 => 2 case ? => 3"
      r3 `shouldSatisfy` isLeft

      r4 <- parseTest' eSwitch "switch 1 { case 1 => 1 case 2 => 2 case ? => 3 4 }"
      r4 `shouldSatisfy` isLeft