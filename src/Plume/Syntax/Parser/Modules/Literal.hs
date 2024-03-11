module Plume.Syntax.Parser.Modules.Literal where

import Control.Monad.Parser
import Data.Text qualified as T
import Plume.Syntax.Concrete
import Plume.Syntax.Parser.Lexer
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

parseLiteral :: Parser Expression -> Parser Expression
parseLiteral f =
  choice
    [ ELiteral <$> parseChar
    , stringLiteralInterpolated f
    , ELiteral <$> try parseFloat
    , ELiteral <$> parseBool
    , ELiteral <$> parseInteger
    ]

-- Parser utility functions
-- Used to parse for instance string sequences which will be later
-- encapsulated into their CST correspondance

charLiteral :: Parser Char
charLiteral = sameLine $ lexeme $ between (char '\'') (char '\'') $ do
  c <- L.charLiteral
  guard (c /= '\'')
  return c

stringLiteral :: Parser Text
stringLiteral =
  lexeme $
    T.pack
      <$> (char '"' *> manyTill L.charLiteral (char '"'))

buildString :: [Expression] -> Expression
buildString [x] = x
buildString (x : xs) = EBinary Plus x (buildString xs)
buildString [] = ELiteral (LString "")

stringLiteralInterpolated :: Parser Expression -> Parser Expression
stringLiteralInterpolated f = lexeme $ do
  str <- char '"' *> manyTill interpolation (char '"')
  return $ buildString str
 where
  parseInterpolation = braces (f <* sc)
  parseCharChunk = do
    str <-
      T.pack
        <$> some
          (notFollowedBy (void parseInterpolation <|> void (char '"')) >> L.charLiteral)
    return $ ELiteral (LString str)

  interpolation = parseInterpolation <|> parseCharChunk

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

-- Actual literal parsing functions

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
