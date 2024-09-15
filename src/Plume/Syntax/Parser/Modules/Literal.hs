module Plume.Syntax.Parser.Modules.Literal where

import Control.Monad.Parser
import Data.Text qualified as T
import Plume.Syntax.Common.Literal
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Concrete
import Plume.Syntax.Parser.Lexer
import Text.Megaparsec hiding (many, parse, some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- | Parse a literal
-- | A literal is a value that is directly represented in the source code
-- |
-- | example: 42, "Hello, World!", true
-- |
-- | Try is used to parse floats in order to backtrack if the float parser fails
-- | and try the integer parser instead.
parseLiteral :: Parser Expression
parseLiteral =
  choice
    [ ELiteral <$> parseChar
    , ELiteral . LRegex <$> parseRegex
    , parseStringWithInterpolation
    , ELiteral <$> try parseFloat
    , ELiteral <$> parseBool
    , ELiteral <$> parseInteger
    ]

parseRegex :: Parser Text
parseRegex = lexeme $ do
  void $ char '/'
  res <- takeWhileP Nothing (/= '/')
  void $ char '/'
  pure res

parseStringWithInterpolation :: Parser Expression
parseStringWithInterpolation = lexeme $ do
  void $ char '"'
  es <- manyTill L.charLiteral (char '"')
  pure . combineCharsIntoString $ buildString es

  where
    showApp :: Expression -> Expression
    showApp x = EApplication (EProperty "show" x) []

    buildString :: [Char] -> Expression
    buildString [] = ELiteral (LString "")
    buildString ('$':x:xs) | isIdentCharStart (T.singleton x) = do
      -- span the variable name
      let (var, rest) = span isIdentChar xs
          rest'       = buildString rest
          var'        = EVariable (fromText $ T.pack (x:var)) Nothing

      EBinary "+" (showApp var') rest'
    buildString (x:xs) = do
      let (rest, next) = span (/= '$') xs
          rest'        = T.pack (x:rest)

      EBinary "+" (ELiteral $ LString rest') (buildString next)

    combineCharsIntoString :: Expression -> Expression
    combineCharsIntoString (EBinary "+" x (ELiteral (LString ""))) = combineCharsIntoString x 
    combineCharsIntoString (EBinary "+" x y) = do
      let x' = combineCharsIntoString x
      let y' = combineCharsIntoString y

      case (x', y') of
        (ELiteral (LString a), ELiteral (LString b)) -> ELiteral (LString (a <> b))
        _ -> EBinary "+" x' y'
    combineCharsIntoString x = x

-- | Parse a character literal
-- | A character literal is a single character enclosed in single quotes
-- | Example: 'a'
-- | note: The character can be escaped
-- |       Example: '\n' is a newline character
-- |       The character can also be unescaped single quote character
charLiteral :: Parser Char
charLiteral = lexeme $ between (char '\'') (char '\'') L.charLiteral

-- | Parse a string literal
-- | A string literal is a sequence of characters enclosed in double quotes
-- | Example: "Hello, World!"
-- | note: The string can contain escaped characters
-- |       Example: "Hello\nWorld" is a string with a newline character
-- |       However, unlike character literals, the string can contain unescaped
-- |       double quote characters.
stringLiteral :: Parser Text
stringLiteral =
  lexeme $
    T.pack
      <$> (char '"' *> manyTill L.charLiteral (char '"'))

-- | Parse an integer literal
-- | Using `signed` combinator to parse signed integers
-- |
-- | example: 42, -42
-- |
-- | The sign must be directly in front of the number as there is no whitespace
-- | accepted between the sign and the number.
integer :: Parser Integer
integer = lexeme (L.signed mempty L.decimal)

-- | Parse a floating point number
-- | Same as integer, we use the `signed` combinator to parse signed
-- | floating point numbers.
float :: Parser Double
float = lexeme (L.signed mempty L.float)

-- Actual literal parsing functions

parseString :: Parser Literal
parseString = LString <$> stringLiteral

parseChar :: Parser Literal
parseChar = LChar <$> charLiteral

parseInteger :: Parser Literal
parseInteger = LInt <$> integer

parseFloat :: Parser Literal
parseFloat = LFloat <$> float

parseBool :: Parser Literal
parseBool =
  choice
    [ LBool True <$ symbol "true"
    , LBool False <$ symbol "false"
    ]
