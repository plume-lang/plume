module Plume.Syntax.Parser.Modules.ParseImports where

import Control.Monad.Parser
import Plume.Syntax.Concrete.Expression (Position)
import Plume.Syntax.Parser.Lexer
import Plume.Syntax.Parser.Modules.Literal
import Text.Megaparsec hiding (many)

eRequire :: Parser TempAST
eRequire = do
  p1 <- getSourcePos
  void $ symbol "require"
  path <- stringLiteral
  p2 <- getSourcePos
  pure $ Import path (p1, p2)

data TempAST
  = Import Text Position
  | Other

parseImports :: Parser [TempAST]
parseImports = many (eRequire <|> (anySingle $> Other))

getRequire :: [TempAST] -> [(Text, Maybe Position)]
getRequire = foldr f []
 where
  f (Import t p) acc = (t, Just p) : acc
  f _ acc = acc

getPaths :: Parser [(Text, Maybe Position)]
getPaths = getRequire <$> parseImports