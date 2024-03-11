module Plume.Syntax.Parser.Modules.Pattern (parsePattern) where

import Control.Monad.Parser
import Plume.Syntax.Concrete
import Plume.Syntax.Parser.Lexer
import Plume.Syntax.Parser.Modules.Literal hiding (parseLiteral)
import Text.Megaparsec

parsePattern :: Parser ConcretePattern
parsePattern =
  choice
    [ parseWildcard
    , try parseConstructor
    , parseVariable
    , parseLiteral
    ]

parseVariable :: Parser ConcretePattern
parseVariable = PVariable <$> identifier

parseLiteral :: Parser ConcretePattern
parseLiteral =
  PLiteral
    <$> choice
      [ parseString
      , parseChar
      , parseBool
      , try parseFloat
      , parseInteger
      ]

parseConstructor :: Parser ConcretePattern
parseConstructor = do
  name <- identifier
  args <- parens (parsePattern `sepBy` comma)
  return $ PConstructor name args

parseWildcard :: Parser ConcretePattern
parseWildcard = PWildcard <$ symbol "?"
