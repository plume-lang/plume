module Control.Monad.Parser (
  module IO,
  Parser,
  FileContent,
  ParsingError,
  parse,
  parseWithRef,
  parseWithRefValue,
) where

import Control.Monad.IO as IO
import Text.Megaparsec hiding (parse)

type Parser = ParsecT Void Text IO

type FileContent = Text

type ParsingError = ParseErrorBundle FileContent Void

instance Semigroup Int where (<>) = (+)

instance Monoid Int where mempty = 0

parse
  :: Parser a
  -> FilePath
  -> FileContent
  -> IO (Either ParsingError a)
parse p filePath fileContent = do
  runParserT
    p
    filePath
    fileContent

parseWithRef
  :: (Monoid ref)
  => IORef ref
  -> Parser a
  -> FilePath
  -> FileContent
  -> IO (Either ParsingError a)
parseWithRef ref = parseWithRefValue (ref, mempty)

parseWithRefValue
  :: (IORef ref, ref)
  -> Parser a
  -> FilePath
  -> FileContent
  -> IO (Either ParsingError a)
parseWithRefValue (ref, value) p filePath fileContent = do
  writeIORef ref value
  r <-
    runParserT
      p
      filePath
      fileContent
  writeIORef ref value
  return r
