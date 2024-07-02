module Language.Plume.Frontend.Parser.Expression where

import Language.Plume.Frontend.Parser qualified as P
import Language.Plume.Frontend.Parser.Lexer qualified as Lex
import Language.Plume.Syntax.HLIR qualified as HLIR
import Control.Monad.Combinators.Expr qualified as P
import Text.Megaparsec.Char qualified as MC
import Data.Foldable qualified as Fold

import Language.Plume.Frontend.Parser.Internal.Literal qualified as Lit
import Language.Plume.Frontend.Parser.Internal.Position qualified as Loc
import Language.Plume.Frontend.Parser.Internal.Annotation qualified as Ann

-- Make a unary operator sequence
-- A unary operator sequence is a sequence of unary operators
-- that are applied to an expression.
--
-- example: not not a <=> not (not a)
--
-- The sequence is applied from right to left.
-- This is because the postfix and prefix operators are applied *
-- from right to left.
makeUnaryOp :: (Alternative f) => f (a -> a) -> f (a -> a)
makeUnaryOp s = Fold.foldr1 (.) . reverse <$> some s

parseExprVar :: (MonadIO m) => P.Parser m (HLIR.AST "expression")
parseExprVar = Loc.localize $ do
  name <- Lex.identifier
  pure $ HLIR.MkExprVariable (HLIR.MkAnnotation name Nothing)

parseExprLit :: (MonadIO m) => P.Parser m (HLIR.AST "expression")
parseExprLit =
  P.between Lex.scn Lex.scn $
    Loc.localize (HLIR.MkExprLiteral <$> Lit.parseLiteral)

parseExprReturn :: (MonadIO m) => P.Parser m (HLIR.AST "expression")
parseExprReturn = Loc.localize $ do
  void $ Lex.reserved "return"
  HLIR.MkExprReturn <$> parseExpression

parseExprLambda :: (MonadIO m) => P.Parser m (HLIR.AST "expression")
parseExprLambda = Loc.localize $ do
  void $ Lex.reserved "fn"
  args <- Lex.parens (Ann.annotate Ann.annotatedType `P.sepBy` Lex.comma)
  ret <- Ann.annotatedType

  HLIR.MkExprLambda args ret <$> parseBlockOrExpr

parseExprIf :: (MonadIO m) => P.Parser m (HLIR.AST "expression")
parseExprIf = Loc.localize $ do
  void $ Lex.reserved "if"
  cond <- parseExpression
  void $ Lex.reserved "then"
  thenBranch <- parseExpression
  void $ Lex.reserved "else"

  HLIR.MkExprIf cond thenBranch <$> parseExpression

parseTerm :: (MonadIO m) => P.Parser m (HLIR.AST "expression")
parseTerm = P.choice
  [ parseExprLit,
    parseExprLambda,
    parseExprIf,
    parseExprReturn,
    parseExprVar,
    Lex.parens parseExpression
  ]

parseExpression :: (MonadIO m) => P.Parser m (HLIR.AST "expression")
parseExpression = P.makeExprParser parseTerm table
  where
    table =
      [
        [ P.Postfix $
          makeUnaryOp $ do
            args <- Lex.parens (parseExpression `P.sepBy` Lex.comma)
            pure $ \e -> HLIR.MkExprCall e args Nothing
        ],
        [
          P.InfixL (HLIR.MkExprBinary "+" <$ Lex.symbol "+"),
          P.InfixL (HLIR.MkExprBinary "-" <$ Lex.symbol "-")
        ],
        [
          P.InfixL (HLIR.MkExprBinary "*" <$ Lex.symbol "*"),
          P.InfixL (HLIR.MkExprBinary "/" <$ Lex.symbol "/")
        ],
        [
          P.InfixL (HLIR.MkExprBinary "==" <$ Lex.symbol "=="),
          P.InfixL (HLIR.MkExprBinary "!=" <$ Lex.symbol "!=")
        ],
        [
          P.InfixL (HLIR.MkExprBinary "and" <$ Lex.symbol "and"),
          P.InfixL (HLIR.MkExprBinary "or" <$ Lex.symbol "or")
        ],
        [
          P.Prefix $
            makeUnaryOp $
              HLIR.MkExprUnary "-" <$ Lex.symbol "-"
        ]
      ]

parseBlockOrExpr :: (MonadIO m) => P.Parser m (HLIR.AST "expression")
parseBlockOrExpr = P.choice [
    flip HLIR.MkExprBlock Nothing <$>
      Lex.braces (parseExpression `P.sepEndBy` endOfLine),
    Lex.symbol "=>" *> parseExpression
  ]

endOfLine :: (MonadIO m) => P.Parser m ()
endOfLine = P.option () (P.try Lex.semi $> ()) <|> MC.eol $> ()
