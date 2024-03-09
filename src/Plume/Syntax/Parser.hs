module Plume.Syntax.Parser where

import Control.Monad.Parser
import Plume.Syntax.Concrete
import Plume.Syntax.Parser.Lexer
import Plume.Syntax.Parser.Modules.Expression

parseTestPlume :: FileContent -> IO (Either ParsingError Program)
parseTestPlume = parsePlumeFile mempty

parsePlumeFile :: FilePath -> FileContent -> IO (Either ParsingError Program)
parsePlumeFile =
  parseWithRefValue
    (indentation, 0)
    (scn *> parseProgram)
