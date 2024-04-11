module Plume.Syntax.Parser.Modules.ParseImports where

import Control.Monad.Parser
import Plume.Syntax.Concrete.Expression (Position)
import Plume.Syntax.Parser.Lexer hiding (symbol)
import Plume.Syntax.Parser.Modules.Literal
import Text.Megaparsec hiding (many)
import Text.Megaparsec.Char.Lexer

eRequire :: Parser TempAST
eRequire = nonIndented scn $ do
  p1 <- getSourcePos
  void $ reserved "require"
  path <- stringLiteral
  p2 <- getSourcePos
  pure $ Import path (p1, p2)

data TempAST
  = Import Text Position
  | Other

parseImports :: Parser [TempAST]
parseImports =
  scn
    *> many
      ( choice
          [ try eRequire
          , Other <$ anySingle
          ]
      )
    <* scn

getRequire :: [TempAST] -> [(Text, Maybe Position)]
getRequire = foldr f []
 where
  f (Import t p) acc = (t, Just p) : acc
  f _ acc = acc

getPaths :: Parser [(Text, Maybe Position)]
getPaths = getRequire <$> parseImports