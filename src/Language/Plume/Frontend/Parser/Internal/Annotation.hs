module Language.Plume.Frontend.Parser.Internal.Annotation where

import Language.Plume.Frontend.Parser qualified as P
import Language.Plume.Frontend.Parser.Lexer qualified as Lex
import Language.Plume.Syntax.HLIR qualified as HLIR
import Language.Plume.Frontend.Parser.Internal.Type qualified as Ty

annotate :: (MonadIO m) => P.Parser m a -> P.Parser m (HLIR.Annotation a)
annotate p = HLIR.MkAnnotation <$> Lex.identifier <*> p

annotate' :: (MonadIO m) => P.Parser m a -> P.Parser m a
annotate' p = Lex.identifier *> p

annotatedType :: MonadIO m => P.Parser m (Maybe HLIR.PlumeType)
annotatedType = P.optional $ do
  void Lex.colon
  Ty.parseType

annotatedType' :: MonadIO m => P.Parser m HLIR.PlumeType
annotatedType' = void Lex.colon *> Ty.parseType
