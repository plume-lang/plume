module Plume.Syntax.Parser where

import Control.Monad.Parser
import Plume.Syntax.Concrete
import Plume.Syntax.Parser.Lexer
import Plume.Syntax.Parser.Parser
import Data.SortedList qualified as SL

parseTestPlume
  :: FileContent
  -> IO (Either ParsingError (Program, SL.SortedList CustomOperator))
parseTestPlume fc = parsePlumeFile mempty fc mempty

parsePlumeFile
  :: FilePath
  -> FileContent
  -> SL.SortedList CustomOperator
  -> IO (Either ParsingError (Program, SL.SortedList CustomOperator))
parsePlumeFile fp fc ops' = do
  modifyIORef' customOperators (ops' `SL.union`)
  res <- parse parseProgram fp fc
  ops <- readIORef customOperators
  pure $ (,ops) <$> res
