module Language.Plume.Frontend.Parser.Internal.Pattern where

import Language.Plume.Frontend.Parser qualified as P
import Language.Plume.Frontend.Parser.Lexer qualified as Lex
import Language.Plume.Syntax.HLIR qualified as HLIR
import Language.Plume.Frontend.Parser.Internal.Literal qualified as Lit
import Language.Plume.Frontend.Parser.Internal.Position qualified as Pos

parsePatVar :: (MonadIO m) => P.Parser m (HLIR.AST "pattern")
parsePatVar = do
  name <- Lex.identifier
  pure $ HLIR.MkPatternVariable (HLIR.MkAnnotation name Nothing)

parsePatWildcard :: (MonadIO m) => P.Parser m (HLIR.AST "pattern")
parsePatWildcard = do
  _ <- Lex.symbol "?"
  pure $ HLIR.MkPatternWildcard Nothing

parsePatLiteral :: (MonadIO m) => P.Parser m (HLIR.AST "pattern")
parsePatLiteral = HLIR.MkPatternLiteral <$> Lit.parseLiteral

parsePattern :: (MonadIO m) => P.Parser m (HLIR.AST "pattern")
parsePattern = P.choice [
    parsePatLiteral,
    parsePatWildcard,
    parsePatVar,
    Pos.localize parsePattern
  ]