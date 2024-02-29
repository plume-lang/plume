module Language.Feather.Parser.Modules.Literal where

import Data.Functor
import Language.Feather.CST.Literal
import Language.Feather.Parser.Lexer
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

parseLiteral :: Parser Literal
parseLiteral =
  choice
    [ parseChar,
      parseString,
      try parseFloat,
      parseInteger
    ]

-- Parser utility functions
-- Used to parse for instance string sequences which will be later
-- encapsulated into their CST correspondance

charLiteral :: Parser Char
charLiteral = lexeme $ between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = lexeme $ char '"' *> manyTill L.charLiteral (char '"')

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

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
  ( reserved "true"
      $> LBool True
  )
    <|> ( reserved "false"
            $> LBool False
        )
