module Control.Monad.Parser (
  module IO,
  Parser,
  FileContent,
  ParsingError,
  parse,
) where

import Control.Monad.IO as IO
import Text.Megaparsec hiding (parse)

type Parser = ParsecT Void Text (ReaderT Int IO)

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
  runReaderT (runParserT p filePath fileContent) 1
