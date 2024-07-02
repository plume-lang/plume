module Language.Plume.Frontend.Parser (
    module P,
    module M,
    module MCL,
    ParseError,
    parsePlumeFile,
    parsePlumeTestFile,
) where

import Control.Monad.Parser as P
import Text.Megaparsec as M hiding (ParseError)
import Text.Megaparsec.Char.Lexer as MCL

type ParseError = ParseErrorBundle Text Void

parsePlumeFile ::
    (MonadIO m) =>
    FilePath ->
    FileContent ->
    P.Parser m a ->
    m (Either ParseError a)
parsePlumeFile filePath fileContent p =
    P.parseContent p filePath fileContent

parsePlumeTestFile ::
    (MonadIO m) =>
    FileContent ->
    P.Parser m a ->
    m (Either ParseError a)
parsePlumeTestFile = flip P.parseTestContent
