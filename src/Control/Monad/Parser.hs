module Control.Monad.Parser (
  module IO,
  Parser,
  FileContent,
  ParsingError,
  parse,
) where

import Control.Monad.IO as IO
import Text.Megaparsec hiding (parse)

type Parser = ParsecT Void Text IO

type FileContent = Text

type ParsingError = ParseErrorBundle FileContent Void

{-# INLINE parse #-}
parse
  :: Parser a
  -> FilePath
  -> FileContent
  -> IO (Either ParsingError a)
parse p filePath fileContent = do
  runParserT p filePath fileContent
