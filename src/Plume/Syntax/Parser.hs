module Plume.Syntax.Parser where

import Plume.Syntax.Concrete
import Plume.Syntax.Parser.Lexer
import Plume.Syntax.Parser.Modules.Expression
import Text.Megaparsec

type FileContent = Text

type ParsingError = ParseErrorBundle FileContent Void

parseTest :: FileContent -> Either ParsingError Program
parseTest s = parsePlumeFile "" s

parsePlumeFile :: FilePath -> FileContent -> Either ParsingError Program
parsePlumeFile filePath fileContent =
  fst $
    runState (runParserT (scn *> parseProgram) filePath fileContent) 0
