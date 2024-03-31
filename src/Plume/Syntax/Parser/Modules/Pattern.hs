module Plume.Syntax.Parser.Modules.Pattern (parsePattern) where

import Control.Monad.Parser
import Plume.Syntax.Common.Pattern
import Plume.Syntax.Parser.Lexer
import Plume.Syntax.Parser.Modules.Literal hiding (parseLiteral)
import Text.Megaparsec hiding (some)

parsePattern :: Parser Pattern
parsePattern =
  choice
    [ parseWildcard
    , parseList
    , try parseConstructor
    , parseVariable
    , parseLiteral
    ]

parseVariable :: Parser Pattern
parseVariable = PVariable <$> identifier

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

parseList :: Parser Pattern
parseList =
  brackets $ do
    items <- (try parseSlice <|> parsePattern) `sepBy` comma
    let (items', slice) = case reverse items of
          [] -> ([], Nothing)
          (p@(PSlice _) : rest) -> (reverse rest, Just p)
          _ -> (items, Nothing)
    return $ PList items' slice

parseSlice :: Parser Pattern
parseSlice = PSlice <$> (symbol ".." *> identifier)

parseConstructor :: Parser Pattern
parseConstructor = do
  name <- identifier
  args <- parens (parsePattern `sepBy` comma)
  return $ PConstructor name args

parseWildcard :: Parser Pattern
parseWildcard = PWildcard <$ symbol "?"
