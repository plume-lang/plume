{-# LANGUAGE BlockArguments #-}

module Language.Feather.Parser.Modules.Expression where

import Control.Monad.Combinators.Expr
import Control.Monad.State
import Data.Maybe
import Language.Feather.CST
import Language.Feather.Parser.Lexer
import Language.Feather.Parser.Modules.Literal
import Language.Feather.Parser.Modules.Operator
import Language.Feather.Parser.Modules.Type
import Text.Megaparsec

-- Some useful parsing functions

-- Used to parse location and encapsulate parsed output into Located expression.
-- It stores starting and ending positions of the expression
eLocated :: Parser Expression -> Parser Expression
eLocated p = do
  start <- getSourcePos
  res <- p
  end <- getSourcePos
  return (res :>: (start, end))

-- Used to parse variable annotations such as (x: t)
-- where x is an identifier and t is a concrete type
annotated :: Parser (Annotation (Maybe ConcreteType))
annotated = Annotation <$> identifier <*> ty
  where
    ty = optional (symbol ":" *> tType)

-- Used to parse a block of expressions
-- This takes care of indentation and newlines
-- Block either starts with a new line and indentations or
-- it is a single line expression
indentOrInline :: Parser Expression -> Parser Expression
indentOrInline p = do
  eBlock p <|> eLocated p

-- Actual parsing functions

eLiteral :: Parser Expression
eLiteral = ELiteral <$> parseLiteral <?> "literal"

-- { e1; e2; ...; en } where e1, e2, ..., en are expressions
eBlock :: Parser Expression -> Parser Expression
eBlock p = eLocated $ do
  bl <- indent p
  case bl of
    [] -> fail "Empty block"
    [x] -> return x
    _ -> return (EBlock bl)

-- (e) where e is an expression
eParenthized :: Parser Expression
eParenthized = parens eExpression

-- All variables must be either alpha-numerical or special characters
-- such as: _
-- So variables cannot have spaces, quotes, double quotes, some open-close
-- symbols (like brackets, braces, angles, parenthesis) and unicode characters
eVariable :: Parser Expression
eVariable = eLocated $ EVariable <$> identifier

-- x: t = e1 in e2 where x is variable name (resp. identifier), t is optional
-- variable type, e1 is variable's actual value and e2 is the optional return
-- value of this "statement" (which becomes an expression)
eDeclaration :: Parser Expression
eDeclaration = eLocated $ do
  var <-
    try $
      Annotation
        <$> identifier
        <*> optional (symbol ":" *> tType)
        <* symbol "="
  ilevel <- get
  expr <- indentOrInline eExpression
  body <- optional (indentSameOrInline ilevel $ reserved "in" *> eExpression)
  return (EDeclaration var expr body)

-- if e1 then e2 else e3 where e1 is the condition, e2 is the "then" branch and
-- e2 is the "else" branch
eConditionBranch :: Parser Expression
eConditionBranch = eLocated $ do
  _ <- reserved "if"
  cond <- eExpression
  _ <- reserved "then"
  ilevel <- get
  thenBr <- indentOrInline eExpression
  _ <- indentSameOrInline ilevel $ reserved "else"
  elseBr <- indentOrInline eExpression
  return (EConditionBranch cond thenBr elseBr)

-- (a: t1, b: t2, ..., z: tn): ret -> e where a, b, ..., z are closure
-- arguments, t1, t2, ..., tn are closure optional arguments types, ret is the
-- closure return type and e is the actual closure body. Closures are a
-- generalized form of lambdas, which can be also called anonymous functions.
-- Parenthesis can be omitted when there is only one argument without type
-- specified, which results in the following form: x -> e where x is the
-- closure argument name and e the closure body expression
eClosure :: Parser Expression
eClosure = eLocated $ do
  (args, ret) <- try $ do
    args <- clArguments
    ret <- optional (symbol ":" *> tType)
    _ <- symbol "->"
    return (args, ret)
  body <- indentOrInline eExpression
  return (EClosure args ret body)
  where
    clArguments =
      choice
        [ clMonoArg,
          clPolyArg
        ]
    clMonoArg = pure <$> (Annotation <$> identifier <*> pure Nothing)
    clPolyArg = parens (annotated `sepBy` comma)

-- name(a: t1, b: t2, ..., z: tn): ret -> e where name is the function name,
-- parenthesized elements are function arguments, ret is function return type
-- and e function body. This is a sugared form that combines both variable
-- declaration and closure expression
eFunctionDefinition :: Parser Expression
eFunctionDefinition = eLocated $ do
  name <- identifier
  arguments <- parens (annotated `sepBy` comma)
  ret <- optional (symbol ":" *> tType)
  _ <- symbol "->"
  body <- indentOrInline eExpression
  return (EDeclaration (name :@: Nothing) (EClosure arguments ret body) Nothing)

-- { l1: e1, l2: 2, ..., ln: en | r } where l1, l2, ..., ln are record labels
-- and e1, e2, ..., en are record values. Record is a data structure that
-- stores key-value pairs. It is similar to a map in other languages but values
-- are heterogeneous. Labels are just identifiers (as variables).
eRecord :: Parser Expression
eRecord = eLocated . braces $ do
  fields <-
    (,)
      -- Parse record fields
      <$> ( ( do
                name <- identifier
                _ <- symbol ":"
                expr <- eExpression
                return (name, expr)
            )
              `sepBy` comma
          )
      -- Parse optional record extension
      <*> optional (symbol "|" *> eExpression)

  return $ case fields of
    ([], _) -> ERowEmpty
    (f, r) ->
      foldr
        (\(name, expr) acc -> ERowExtension name expr acc)
        (fromMaybe ERowEmpty r)
        f

-- Main expression parsing function
eExpression :: Parser Expression
eExpression = makeExprParser eTerm ([postfixOperators] : operators)
  where
    eTerm =
      choice
        [ eLiteral,
          eRecord,
          -- Try may be used here because function definitions starts with the
          -- same syntax as variable declaration
          try eFunctionDefinition,
          eClosure,
          eConditionBranch,
          -- Try may be used here because declaration starts with the same
          -- syntax as equality operator (for instance in parsing x == 5, this
          -- may be conflicting with x = ..., where ... would be "= 5")
          try eDeclaration,
          eVariable,
          eParenthized
        ]
    -- Extending operators in order to add function call support. A function
    -- call is just a postfix operator where operand is the callee and operator
    -- is the actual arguments associated to the function call. A function call
    -- is actually an expression of the following form:
    -- e0(e1, e2, e3) where e_n are basically expressions
    --
    -- MakeUnaryOp lets the parser knowing that this operator may be chained,
    -- in particular for closure calls: x(4)(5)(6)
    postfixOperators =
      Postfix $
        makeUnaryOp $
          choice
            [ do
                arguments <- do
                  _ <- symbol "("
                  ilevel <- get

                  -- Parsing function call args either inlined or indented
                  args <-
                    indentSepBy eExpression comma
                      <|> (eExpression `sepBy` comma)

                  _ <- indentSameOrInline ilevel $ symbol ")"
                  return args

                -- Optional syntaxic sugar for callback argument
                lambdaArg <- optional $ do
                  _ <- symbol "->"
                  body <- indentOrInline eExpression
                  return $ EClosure [] Nothing body

                return \e -> EApplication e (arguments ++ maybeToList lambdaArg),
              -- Record restriction e \ x where e may be a record and x a label
              -- to "remove" from the record
              do
                _ <- symbol "\\"
                name <- identifier
                return $ \e -> ERowRestrict e name,
              -- Record selection e.x where e may be a record and x a label to
              -- select from the record
              do
                _ <- symbol "."
                name <- identifier
                return $ \e -> ERowSelect e name
            ]

parseProgram :: Parser Program
parseProgram = many (nonIndented eExpression <* optional indentSc)
