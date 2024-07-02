module Control.Monad.Parser (
  module IO,
  Parser,
  FileContent,
  ParsingError,
  parseContent,
  parseTestContent
) where

import Text.Megaparsec hiding (parse, parseTest)
import GHC.IO qualified as IO

type Parser m a = ParsecT Void Text m a

type FileContent = Text

type ParsingError = ParseErrorBundle FileContent Void

{-# INLINE parseContent #-}
parseContent
  :: (MonadIO m)
  => Parser m a
  -> FilePath
  -> FileContent
  -> m (Either ParsingError a)
parseContent p filePath fileContent = do
  runParserT p filePath fileContent

{-# INLINE parseTestContent #-}
parseTestContent
    :: (MonadIO m)
    => Parser m a
    -> FileContent
    -> m (Either ParsingError a)
parseTestContent p = parseContent (p <* eof) "<test>"
