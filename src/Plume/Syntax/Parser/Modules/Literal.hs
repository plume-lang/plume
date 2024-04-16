module Plume.Syntax.Parser.Modules.Literal where

import Control.Monad.Parser
import Data.Text qualified as T
import Plume.Syntax.Common.Literal
import Plume.Syntax.Concrete
import Plume.Syntax.Parser.Lexer
import Text.Megaparsec hiding (many, some)
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
  ELiteral
    <$> choice
      [ parseChar
      , parseString
      , try parseFloat
      , parseBool
      , parseInteger
      ]

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
