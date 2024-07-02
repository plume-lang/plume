module Language.Plume.Frontend.Parser.Internal.Literal where

import Language.Plume.Frontend.Parser qualified as P
import Language.Plume.Syntax.Internal.Literal qualified as Lit
import Text.Megaparsec.Char qualified as MC
import Data.Text qualified as T

parseInteger :: (MonadIO m) => P.Parser m Integer
parseInteger = P.signed (pure ()) P.decimal

parseFloat :: (MonadIO m) => P.Parser m Double
parseFloat = P.signed (pure ()) P.float

parseChar :: (MonadIO m) => P.Parser m Char
parseChar = MC.char '\'' *> P.charLiteral <* MC.char '\''

parseString :: (MonadIO m) => P.Parser m Text
parseString = MC.char '"' *> P.manyTill P.charLiteral (MC.char '"') <&> T.pack

parseLiteral :: (MonadIO m) => P.Parser m Lit.Literal
parseLiteral =
  P.choice
    [ Lit.MkInteger <$> parseInteger,
      Lit.MkFloat <$> parseFloat,
      Lit.MkChar <$> parseChar,
      Lit.MkString <$> parseString
    ]
