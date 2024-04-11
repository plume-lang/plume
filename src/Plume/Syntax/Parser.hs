module Plume.Syntax.Parser where

import Control.Monad.Parser
import Plume.Syntax.Concrete
import Plume.Syntax.Parser.Lexer
import Plume.Syntax.Parser.Parser

parseTestPlume
  :: FileContent
  -> IO (Either ParsingError (Program, [CustomOperator]))
parseTestPlume fc = parsePlumeFile mempty fc mempty

parsePlumeFile
  :: FilePath
  -> FileContent
  -> [CustomOperator]
  -> IO (Either ParsingError (Program, [CustomOperator]))
parsePlumeFile fp fc ops' = do
  modifyIORef' customOperators (ops' ++)
  res <- parse parseProgram fp fc
  ops <- readIORef customOperators
  pure $ (,ops) <$> res
