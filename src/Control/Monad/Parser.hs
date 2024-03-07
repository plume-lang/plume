module Control.Monad.Parser (
  module IO,
  Parser,
  FileContent,
  ParsingError,
  parse,
  parseWithRef,
) where

import Control.Monad.IO as IO
import Text.Megaparsec hiding (parse)

type Parser = ParsecT Void Text IO

type FileContent = Text

type ParsingError = ParseErrorBundle FileContent Void

instance Semigroup Int where (<>) = (+)

instance Monoid Int where mempty = 0

parse ::
  Parser a ->
  FilePath ->
  FileContent ->
  IO (Either ParsingError a)
parse p filePath fileContent = do
  runParserT
    p
    filePath
    fileContent

parseWithRef ::
  (Monoid ref) =>
  IORef ref ->
  Parser a ->
  FilePath ->
  FileContent ->
  IO (Either ParsingError a)
parseWithRef ref p filePath fileContent = do
  r <-
    runParserT
      p
      filePath
      fileContent
  delete ref
  return r