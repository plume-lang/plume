module Language.Feather.Parser where

import Language.Feather.CST
import Language.Feather.Parser.Lexer
import Language.Feather.Parser.Modules.Expression
import Text.Megaparsec

type FileContent = Text

type ParsingError = ParseErrorBundle FileContent Void

parseTest :: FileContent -> Either ParsingError Program
parseTest s = parseFeatherFile "" s

parseFeatherFile :: FilePath -> FileContent -> Either ParsingError Program
parseFeatherFile filePath fileContent =
  fst $
    runState (runParserT (scn *> parseProgram) filePath fileContent) 0
