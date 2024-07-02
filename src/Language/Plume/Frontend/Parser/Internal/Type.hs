module Language.Plume.Frontend.Parser.Internal.Type where

import Language.Plume.Frontend.Parser qualified as P
import Language.Plume.Syntax.Internal.Type qualified as Ty
import Language.Plume.Frontend.Parser.Lexer qualified as Lex

parseTypeId :: (MonadIO m) => P.Parser m Ty.PlumeType
parseTypeId = Ty.MkTyId <$> Lex.identifier

parseTypeApp :: (MonadIO m) => P.Parser m Ty.PlumeType
parseTypeApp = do
  tyId <- parseTypeId
  tyArgs <- Lex.angles (parseType `P.sepBy1` Lex.comma)
  pure $ Ty.MkTyApp tyId tyArgs

parseType :: (MonadIO m) => P.Parser m Ty.PlumeType
parseType = P.choice [P.try parseTypeApp, parseTypeId]
