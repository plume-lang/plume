module Plume.Syntax.Parser.Modules.ParseImports where

import Control.Monad.Parser
import Plume.Syntax.Concrete.Expression (Position)
import Plume.Syntax.Parser.Lexer hiding (symbol)
import Plume.Syntax.Parser.Modules.Literal
import Text.Megaparsec hiding (many)

-- | Temporary AST used to parse imports
-- | Only `Import` are kept 
data TempAST
  = Import Text Position
  | Other

-- | Parse a require statement
-- | A require statement is a statement that imports a module
-- | It is used in this module to get all imports in a file
eRequire :: Parser TempAST
eRequire = lexeme $ do
  p1 <- getSourcePos
  void $ reserved "require"
  path <- stringLiteral
  p2 <- getSourcePos
  pure $ Import path (p1, p2)

-- | Parse all imports in a file
-- | The parser skips everything except require statements
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

-- | Get all imports from a list of `TempAST` and skip everything else
getRequire :: [TempAST] -> [(Text, Maybe Position)]
getRequire = foldr f []
 where
  f (Import t p) acc = (t, Just p) : acc
  f _ acc = acc

-- | Get all paths from a file
getPaths :: Parser [(Text, Maybe Position)]
getPaths = getRequire <$> parseImports