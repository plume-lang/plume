module Plume.Syntax.Parser.Requires where

import Control.Monad.Parser
import Plume.Syntax.Concrete.Expression (Position)
import Plume.Syntax.Parser.Lexer hiding (symbol)
import Plume.Syntax.Parser.Modules.Literal
import Text.Megaparsec hiding (many, some)
import Data.SortedList qualified as SL
import Data.Foldable qualified as Fold

-- | Temporary AST used to parse imports
-- | Only `Import` are kept 
data TempAST
  = Import Text Position Bool
  | Other

data TempASTOperator 
  = Operator CustomOperator
  | OtherOperator

-- | Parse a require statement
-- | A require statement is a statement that imports a module
-- | It is used in this module to get all imports in a file
eRequire :: Parser TempAST
eRequire = lexeme $ do
  p1 <- getSourcePos
  isPub <- isJust <$> optional (reserved "pub")
  void $ reserved "require"
  path <- stringLiteral
  p2 <- getSourcePos
  pure $ Import path (p1, p2) isPub

-- | Parse all imports in a file
-- | The parser skips everything except require statements
parseImports :: Parser [TempAST]
parseImports =
  scn
    *> sepEndBy1
      ( choice
          [ try eRequire
          , Other <$ stringLiteral
          , Other <$ anySingle
          ]
      )
      scn
    <* scn

-- | Get all imports from a list of `TempAST` and skip everything else
getRequire :: [TempAST] -> [(Text, Maybe Position, Bool)]
getRequire = foldr f []
 where
  f (Import t p b) acc = (t, Just p, b) : acc
  f _ acc = acc

-- | Get all paths from a file
getPaths :: Parser [(Text, Maybe Position, Bool)]
getPaths = getRequire <$> parseImports

parseOperator :: Parser [CustomOperator]
parseOperator = do
  opTy <-
    choice
      [ reserved "infixl" $> CInfixL
      , reserved "infixr" $> CInfixR
      , reserved "infix" $> CInfixN
      , reserved "prefix" $> CPrefix
      , reserved "postfix" $> CPostfix
      ]
  prec <- option 0 $ fromInteger <$> integer
  name <- some operator
  let op = SL.toSortedList $ map (\n -> CustomOperator n prec opTy) name
  pure (Fold.toList op)

parseOperators :: Parser (SL.SortedList CustomOperator)
parseOperators = do
  content <- scn *>
    sepEndBy1 
      ( choice
          [ try ((Operator <$>) <$> parseOperator)
          , [OtherOperator] <$ stringLiteral
          , [OtherOperator] <$ anySingle
          ]
      )
      scn
      <* scn
  let content' = concat content
  pure $ buildCustom content'

  where
    buildCustom :: [TempASTOperator] -> SL.SortedList CustomOperator
    buildCustom = Fold.foldl' f mempty
      where
        f acc (Operator op) = SL.union acc (SL.toSortedList [op])
        f acc _ = acc