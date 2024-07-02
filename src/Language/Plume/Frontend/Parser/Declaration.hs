module Language.Plume.Frontend.Parser.Declaration where

import Language.Plume.Frontend.Parser qualified as P
import Language.Plume.Frontend.Parser.Lexer qualified as Lex
import Language.Plume.Syntax.HLIR qualified as HLIR

import Language.Plume.Frontend.Parser.Internal.Literal qualified as Lit
import Language.Plume.Frontend.Parser.Internal.Position qualified as Loc
import Language.Plume.Frontend.Parser.Internal.Annotation qualified as Ann

import Language.Plume.Frontend.Parser.Expression qualified as Expr

parseDeclFunc :: (MonadIO m) => P.Parser m (HLIR.AST "declaration")
parseDeclFunc = do
  void $ Lex.reserved "fn"
  name <- Lex.identifier
  generics <- P.option [] $ Lex.angles (Lex.identifier `P.sepBy` Lex.comma)
  let generics' = map HLIR.MkTyId generics
  args <- Lex.parens (Ann.annotate Ann.annotatedType `P.sepBy` Lex.comma)
  ret <- Ann.annotatedType

  HLIR.MkDeclFunction name generics' args ret <$> Expr.parseBlockOrExpr

parseDeclVar :: (MonadIO m) => P.Parser m (HLIR.AST "declaration")
parseDeclVar = do
  ann <- P.try $ Ann.annotate Ann.annotatedType <* Lex.symbol "="

  HLIR.MkDeclVariable ann [] <$> Expr.parseExpression

parseDeclRequire :: (MonadIO m) => P.Parser m (HLIR.AST "declaration")
parseDeclRequire = do
  void $ Lex.reserved "require"
  HLIR.MkDeclRequire <$> Lit.parseString

parseDeclPublic :: (MonadIO m) => P.Parser m (HLIR.AST "declaration")
parseDeclPublic = do
  void $ Lex.reserved "pub"
  HLIR.MkDeclPublic <$> parseDeclaration

parseDeclNative :: (MonadIO m) => P.Parser m (HLIR.AST "declaration")
parseDeclNative = do
  void $ Lex.reserved "native"

  name <- Lex.identifier
  generics <- P.option [] $ Lex.angles (Lex.identifier `P.sepBy` Lex.comma)
  let generics' = map HLIR.MkTyId generics
  args <- Lex.parens (Ann.annotate' Ann.annotatedType' `P.sepBy` Lex.comma)

  HLIR.MkDeclNative name generics' args <$> Ann.annotatedType'

parseDeclaration :: (MonadIO m) => P.Parser m (HLIR.AST "declaration")
parseDeclaration = P.choice
  [ parseDeclFunc,
    parseDeclVar,
    parseDeclNative,
    parseDeclRequire,
    parseDeclPublic
  ]

parseProgram :: (MonadIO m) => P.Parser m [HLIR.AST "declaration"]
parseProgram =
  Lex.scn *>
    P.sepEndBy (Loc.localize parseDeclaration) Lex.scn
  <* P.eof
