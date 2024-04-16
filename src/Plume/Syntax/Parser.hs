module Plume.Syntax.Parser where

import Control.Monad.Parser
import Plume.Syntax.Concrete
import Plume.Syntax.Parser.Lexer
import Plume.Syntax.Parser.Parser
import Data.SortedList qualified as SL

type ParseResult a = Either ParsingError (a, SL.SortedList CustomOperator)

-- | Test parse a Plume program
-- | Used to test the parser without the need of defining every meta
-- | information around the program
parseTestPlume
  :: FileContent
  -> IO (ParseResult Program)
parseTestPlume fc = parsePlumeFile mempty fc mempty

-- | Parse a Plume program
-- | The parser will parse the program and return the AST
-- | It takes a file path, the file content and a list of custom operators
-- | The custom operators are used to parse the program and are returned
-- | in the result
parsePlumeFile
  :: FilePath
  -> FileContent
  -> SL.SortedList CustomOperator
  -> IO (ParseResult Program)
parsePlumeFile fp fc ops' = do
  modifyIORef' customOperators (ops' `SL.union`)
  res <- parse parseProgram fp fc
  ops <- readIORef customOperators
  pure $ (,ops) <$> res
