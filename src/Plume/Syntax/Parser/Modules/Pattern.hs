module Plume.Syntax.Parser.Modules.Pattern (parsePattern) where

import Control.Monad.Parser
import Plume.Syntax.Common.Pattern
import Plume.Syntax.Parser.Lexer
import Plume.Syntax.Parser.Modules.Literal hiding (parseLiteral)
import Text.Megaparsec

parsePattern :: Parser Pattern
parsePattern =
  choice
    [ parseWildcard
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

parseConstructor :: Parser Pattern
parseConstructor = do
  name <- identifier
  args <- parens (parsePattern `sepBy` comma)
  return $ PConstructor name args

parseWildcard :: Parser Pattern
parseWildcard = PWildcard <$ symbol "?"
