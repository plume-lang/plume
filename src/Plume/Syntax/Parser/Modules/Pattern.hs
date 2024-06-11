module Plume.Syntax.Parser.Modules.Pattern where

import Control.Monad.Parser
import Plume.Syntax.Parser.Lexer
import Plume.Syntax.Parser.Modules.Literal hiding (parseLiteral)
import Text.Megaparsec hiding (some)
import Plume.Syntax.Concrete

-- | Parse a pattern
-- | A pattern is a value that is used to match against a value
-- |
-- | example: x, (x, y), [x, y], 42, "Hello, World!", true, Ok(x)
parsePattern :: Parser Pattern
parsePattern =
  choice
    [ parseWildcard
    , parseTuple
    , parseList
    , parseLiteral
    , parseConstructor
    , parseVariable
    ]

-- | Parse a variable pattern
-- | A variable pattern is a pattern that matches any value
-- | The variable name is used to refer to the value in the pattern
parseVariable :: Parser Pattern
parseVariable = PVariable <$> identifier <*> pure Nothing

-- | Parse a tuple pattern
-- | A tuple pattern is a pattern that matches a tuple
-- | A tuple is a sequence of values enclosed in parentheses
-- |
-- | example: (x, y)
-- |
-- | A tuple pattern can also be a unit tuple, resulting in a parenthesized
-- | pattern.
-- | By default, tuples are right-associative, meaning that (x, y, z) is
-- | equivalent to (x, (y, z))
parseTuple :: Parser Pattern
parseTuple = do
  items <- parens (parsePattern `sepBy` comma)
  return $ buildTuple items
 where
  buildTuple [] = PVariable "unit" Nothing
  buildTuple [x] = x
  buildTuple (x : xs) = PConstructor ("tuple", Nothing) [x, buildTuple xs]

-- | Parse a literal pattern
-- | A literal pattern is a pattern that matches a literal value
parseLiteral :: Parser Pattern
parseLiteral =
  PLiteral
    <$> choice
      [ parseString
      , parseChar
      , parseBool
      , try parseFloat
      , parseInteger
      ]

-- | Parse a list pattern
-- | A list pattern is a pattern that matches a list
-- | It can contains a slice pattern, which is a pattern that matches a slice
-- | of the list, but slice patterns can only appear at the end of the list
-- |
-- | example: [x, y, z], [x, y, ..z]
parseList :: Parser Pattern
parseList =
  brackets $ do
    items <- (parseSlice <|> parsePattern) `sepBy` comma
    (items', slice) <- case reverse items of
      [] -> return ([], Nothing)
      (p@(PSlice _ _) : rest)
        | not (any isSlice rest) -> return (reverse rest, Just p)
      _ | not (any isSlice items) -> return (items, Nothing)
        | otherwise -> fail "invalid slice position"
    return $ PList Nothing items' slice
  where
    isSlice :: Pattern -> Bool
    isSlice (PSlice _ _) = True
    isSlice _ = False

    parseSlice :: Parser Pattern
    parseSlice = PSlice <$> (symbol ".." *> identifier) <*> pure Nothing


-- | Parse a constructor pattern
-- | A constructor pattern is a pattern that matches a type constructor
-- | A constructor pattern should have a name and a list of patterns
-- | that are used to match the constructor arguments
-- |
-- | example: Ok(x), Error(x)
parseConstructor :: Parser Pattern
parseConstructor = do
  name <- try $ identifier <* symbol "("
  args <- parsePattern `sepBy1` comma
  _ <- symbol ")"
  return $ PConstructor (name, Nothing) args

-- | Parse a wildcard pattern
-- | A wildcard pattern is a pattern that matches any value
-- | It is used to ignore a value in a pattern
-- | It can be seen as a variable that is not used
parseWildcard :: Parser Pattern
parseWildcard = PWildcard Nothing <$ symbol "?"
