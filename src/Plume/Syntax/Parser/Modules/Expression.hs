{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}

module Plume.Syntax.Parser.Modules.Expression where

import Control.Monad.Combinators.Expr
import Control.Monad.Parser
import Plume.Syntax.Common
import Plume.Syntax.Concrete
import Plume.Syntax.Parser.Lexer
import Plume.Syntax.Parser.Modules.Literal
import Plume.Syntax.Parser.Modules.Operator
import Plume.Syntax.Parser.Modules.Pattern
import Plume.Syntax.Parser.Modules.Type
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char

-- Some useful parsing functions

-- Used to parse location and encapsulate parsed output into Located expression.
-- It stores starting and ending positions of the expression
eLocated :: Parser Expression -> Parser Expression
eLocated p = do
  start <- getSourcePos
  res <- p
  end <- getSourcePos
  return (res :>: (start, end))

eLocatedMany :: Parser [Expression] -> Parser [Expression]
eLocatedMany p = do
  start <- getSourcePos
  res <- p
  end <- getSourcePos
  return (map (:>: (start, end)) res)

-- Used to parse variable annotations such as (x: t)
-- where x is an identifier and t is a concrete type
annotated :: Parser (Annotation (Maybe PlumeType))
annotated = Annotation <$> identifier <*> ty
 where
  ty = optional (symbol ":" *> tType)

argAnnotated :: Parser (Annotation (Maybe PlumeType, IsMutable))
argAnnotated =
  choice
    [ Annotation <$> (reserved "mut" *> identifier) <*> ((,True) <$> mutTy)
    , Annotation <$> identifier <*> ((,False) <$> ty)
    ]
 where
  ty = optional (symbol ":" *> tType)
  mutTy = optional (symbol ":" *> tType)

-- Used to parse a block of expressions
-- This takes care of indentation and newlines
-- Block either starts with a new line and indentations or
-- it is a single line expression
indentOrInline :: Parser Expression
indentOrInline = do
  eBlock parseStatement <|> eExpression

indentOrInlineGen :: Parser a -> Parser a
indentOrInlineGen p = do
  indentOne p <|> p

-- Actual parsing functions

gGeneric :: Parser PlumeGeneric
gGeneric =
  choice
    [ try gExtends
    , gVar
    ]

gVar :: Parser PlumeGeneric
gVar = GVar <$> identifier

gExtends :: Parser PlumeGeneric
gExtends = do
  var <- identifier
  _ <- reserved "extends"
  GExtends var
    <$> ( parens (identifier `sepBy` comma)
            <|> ((: []) <$> identifier)
        )

-- { e1; e2; ...; en } where e1, e2, ..., en are expressions
eBlock :: Parser Expression -> Parser Expression
eBlock p = eLocated $ do
  bl <- indent p

  case bl of
    [] -> fail "Block should contain at least one expression"
    _ -> return (EBlock bl)

-- (e) where e is an expression
eParenthesized :: Parser Expression
eParenthesized = parens eExpression

-- All variables must be either alpha-numerical or special characters
-- such as: _
-- So variables cannot have spaces, quotes, double quotes, some open-close
-- symbols (like brackets, braces, angles, parenthesis) and unicode characters
eVariable :: Parser Expression
eVariable = EVariable <$> identifier <* notFollowedBy (oneOf forbiddenChars)

forbiddenChars :: [Char]
forbiddenChars = [':']

-- x: t = e1 in e2 where x is variable name (resp. identifier), t is optional
-- variable type, e1 is variable's actual value and e2 is the optional return
-- value of this "statement" (which becomes an expression)
eDeclaration :: Parser Expression
eDeclaration = do
  var <-
    try $
      Annotation
        <$> identifier
        <*> optional (symbol ":" *> tType)
        <* (notFollowedBy "==" <?> "variable declaration")
        <* symbol "="
  ilevel <- ask
  expr <- indentOrInline
  body <- optional (indentSameOrInline ilevel $ reserved "in" *> eExpression)
  return (EDeclaration [] False var expr body)

eMutDeclaration :: Parser Expression
eMutDeclaration = do
  _ <- reserved "mut"
  var <-
    try $
      Annotation
        <$> identifier
        <*> optional (symbol ":" *> tType)
        <* (notFollowedBy "==" <?> "variable declaration")
        <* symbol "="
  ilevel <- ask
  expr <- indentOrInline
  body <- optional (indentSameOrInline ilevel $ reserved "in" *> eExpression)
  return (EDeclaration [] True var expr body)

stmtOrExpr :: Bool -> (Parser a -> Parser (Maybe a))
stmtOrExpr isStatement = if isStatement then optional else (Just <$>)

parser :: Bool -> Parser Expression
parser True = parseStatement
parser False = eExpression

-- if e1 then e2 else e3 where e1 is the condition, e2 is the "then" branch and
-- e2 is the "else" branch
eConditionBranch :: Bool -> Parser Expression
eConditionBranch isStatement = do
  ilevel <- ask
  _ <- reserved "if"
  cond <- eExpression
  _ <- reserved "then"
  thenBr <- indentOrInline
  elseBr <- stmtOrExpr isStatement $ do
    _ <- indentSameOrInline ilevel $ reserved "else"
    indentOrInline
  return (EConditionBranch cond thenBr elseBr)

eList :: Parser Expression
eList =
  EList <$> do
    _ <- symbol "["
    ilevel <- ask
    res <-
      indentSepBy eExpression comma
        <|> (eExpression `sepBy` comma)
    _ <-
      indentSameOrInline ilevel (symbol "]")
        <|> indentSameOrHigher ilevel (symbol "]")
    return res

eReturn :: Parser Expression
eReturn = do
  _ <- reserved "return"
  EReturn <$> eExpression

-- (a: t1, b: t2, ..., z: tn): ret -> e where a, b, ..., z are closure
-- arguments, t1, t2, ..., tn are closure optional arguments types, ret is the
-- closure return type and e is the actual closure body. Closures are a
-- generalized form of lambdas, which can be also called anonymous functions.
-- Parenthesis can be omitted when there is only one argument without type
-- specified, which results in the following form: x -> e where x is the
-- closure argument name and e the closure body expression
eClosure :: Parser Expression
eClosure = do
  (args, ret) <- try $ do
    args <- clArguments
    ret <- optional (symbol ":" *> tType)
    _ <- symbol "=>"
    return (args, ret)
  EClosure args ret <$> indentOrInline
 where
  clArguments =
    choice
      [ clMonoArg
      , clPolyArg
      ]
  clMonoArg = pure <$> (Annotation <$> identifier <*> pure (Nothing, False))
  clPolyArg = parens (argAnnotated `sepBy` comma)

eExtension :: Parser [Expression]
eExtension = do
  _ <- reserved "extend"
  gens <- option [] $ angles (gGeneric `sepBy` comma)
  ty <- parens ((:@:) <$> (identifier <* colon) <*> tType)
  _ <- reserved "with"
  members <- indent eExtensionMember
  return [ETypeExtension gens ty members]

eExtensionMember :: Parser (ExtensionMember PlumeType)
eExtensionMember =
  choice
    [ eExtFunction
    ]

eExtFunction :: Parser (ExtensionMember PlumeType)
eExtFunction = do
  (name, gens, args, ret) <- try $ do
    name <- try identifier <|> (reserved "operator" *> operator)
    gens <- option [] $ angles (gGeneric `sepBy` comma)
    args <- parens (argAnnotated `sepBy` comma)
    ret <- optional (symbol ":" *> tType)
    _ <- symbol "=>"
    return (name, gens, args, ret)
  ExtDeclaration
    gens
    (name :@: Nothing)
    . EClosure args ret
    <$> indentOrInline

eGenericProperty :: Parser Expression
eGenericProperty = do
  _ <- reserved "property"
  gens <- option [] $ angles (gGeneric `sepBy` comma)
  (ty, name) <-
    (,)
      <$> (tType <* char '.')
      <*> (try identifier <|> (reserved "operator" *> operator))
  args <- parens ((optional (identifier >> colon) *> tType) `sepBy` comma)
  ret <- symbol ":" *> tType
  return (EGenericProperty gens name (ty : args) ret)

indentOrInlineTC :: Parser [TypeConstructor PlumeType]
indentOrInlineTC = do
  block <|> ((: []) <$> typeConstructor)
 where
  block = do
    bl <- indent typeConstructor

    case bl of
      [] -> fail "Block should contain at least one expression"
      _ -> return bl

eType :: Parser [Expression]
eType = do
  _ <- reserved "type"
  name <- identifier
  gens <- option [] $ angles (gGeneric `sepBy` comma)
  void $ symbol "="
  (: []) . EType (Annotation name gens) <$> indentOrInlineTC

typeConstructor :: Parser (TypeConstructor PlumeType)
typeConstructor =
  choice
    [ try $ TConstructor <$> identifier <*> parens (tType `sepBy` comma)
    , TVariable <$> identifier
    ]

eNativeFunction :: Parser [Expression]
eNativeFunction = do
  _ <- reserved "native"
  path <- stringLiteral
  name <- identifier
  gens <- option [] (angles (identifier `sepBy` comma))
  args <- parens ((optional (identifier >> colon) *> tType) `sepBy` comma)
  ret <- symbol ":" *> tType
  let funTy = args :->: ret
  return [ENativeFunction path name gens funTy]

eNativeGroup :: Parser [Expression]
eNativeGroup = do
  _ <- reserved "native"
  path <- stringLiteral
  _ <- reserved "with"
  indent (nativeFun path)
 where
  nativeFun path = do
    name <- identifier
    gens <- option [] (angles (identifier `sepBy` comma))
    args <- parens ((optional (identifier >> colon) *> tType) `sepBy` comma)
    ret <- symbol ":" *> tType
    let funTy = args :->: ret
    return (ENativeFunction path name gens funTy)

-- name(a: t1, b: t2, ..., z: tn): ret -> e where name is the function name,
-- parenthesized elements are function arguments, ret is function return type
-- and e function body. This is a sugared form that combines both variable
-- declaration and closure expression
eFunctionDefinition :: Parser Expression
eFunctionDefinition = do
  (name, generics, arguments, ret) <- try $ do
    name <- try identifier <|> (reserved "operator" *> operator)
    generics <- option [] (angles (gGeneric `sepBy` comma))
    arguments <- parens (argAnnotated `sepBy` comma)
    ret <- optional (symbol ":" *> tType)
    _ <- symbol "=>"
    return (name, generics, arguments, ret)
  body <- indentOrInline
  return
    ( EDeclaration
        generics
        False
        (name :@: Nothing)
        (EClosure arguments ret body)
        Nothing
    )

eCasePattern :: Parser (Pattern, Expression)
eCasePattern = do
  _ <- reserved "case"
  pattern' <- parsePattern
  _ <- symbol "=>"
  body <- indentOrInline
  return (pattern', body)

eSwitch :: Parser Expression
eSwitch = do
  _ <- reserved "switch"
  cond <- eExpression
  branches <- indent eCasePattern
  return (ESwitch cond branches)

eMacro :: Parser [Expression]
eMacro = do
  name <- try $ char '@' *> identifier <* symbol "="
  (: []) . EMacro name <$> indentOrInline

eMacroFunction :: Parser [Expression]
eMacroFunction = do
  (name, args) <- try $ do
    name <- char '@' *> identifier
    args <- parens (identifier `sepBy` comma)
    _ <- symbol "=>"
    return (name, args)
  (: []) . EMacroFunction name args <$> indentOrInline

eMacroVariable :: Parser Expression
eMacroVariable = EMacroVariable <$> (char '@' *> identifier)

eMacroApplication :: Parser Expression
eMacroApplication = do
  name <- char '@' *> identifier
  args <- parens (eExpression `sepBy` comma)
  return (EMacroApplication name args)

parseStatement :: Parser Expression
parseStatement =
  eLocated $
    choice
      [ eReturn
      , eConditionBranch True
      , try eMutDeclaration
      , eDeclaration
      , eFunctionDefinition
      , eExpression
      ]

sortCustomOperators :: [CustomOperator] -> [[Operator Parser Expression]]
sortCustomOperators ops = do
  let ops' = sortBy (\x y -> compare x.precedence y.precedence) ops
  map ((: []) . parseOperator) ops'
 where
  parseOperator :: CustomOperator -> Operator Parser Expression
  parseOperator (CustomOperator name _ CPrefix) =
    prefix name (CustomPrefix name)
  parseOperator (CustomOperator name _ CPostfix) =
    postfix name (CustomPostfix name)
  parseOperator (CustomOperator name _ ty)
    | ty `elem` [CInfixN, CInfixL, CInfixR] = case ty of
        CInfixN -> binary name (CustomInfix name)
        CInfixL -> InfixL (CustomInfix name <$ symbol name)
        CInfixR -> InfixR (CustomInfix name <$ symbol name)
    | otherwise =
        error "Invalid operator type"

pattern CustomInfix :: Text -> Expression -> Expression -> Expression
pattern CustomInfix name e1 e2 = EApplication (EVariable name) [e1, e2]

pattern CustomPrefix, CustomPostfix :: Text -> Expression -> Expression
pattern CustomPrefix name e = EApplication (EVariable name) [e]
pattern CustomPostfix name e = EApplication (EVariable name) [e]

eTuple :: Parser Expression
eTuple = do
  tuple <- parens (eExpression `sepBy1` comma)
  return (buildTuple tuple)
 where
  buildTuple [] = EVariable "unit"
  buildTuple [x] = x
  buildTuple (x : xs) = EApplication (EVariable "tuple") [x, buildTuple xs]

-- Main expression parsing function
eExpression :: Parser Expression
eExpression = eLocated $ do
  customOps <- readIORef customOperators
  let customs = sortCustomOperators customOps
  makeExprParser eTerm ([postfixOperators] : customs ++ operators)
 where
  eTerm =
    choice
      [ parseLiteral eExpression
      , eList
      , try eMacroApplication
      , eMacroVariable
      , eSwitch
      , try eClosure
      , eTuple
      , eConditionBranch False
      , eVariable
      , eParenthesized
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
          [ functionCallOperator
          , transformSlice <$> brackets (parseSlice eTerm)
          , -- Record selection e.x where e may be a record and x a label to
            -- select from the record
            EProperty <$> (char '.' *> field <* scn)
          , try $ EProperty <$> indentOne (char '.' *> field)
          ]

parseSlice :: Parser Expression -> Parser Expression
parseSlice eTerm = do
  res <-
    optional . try $
      (,)
        <$> optional eTerm
        <*> (symbol ".." *> optional eTerm)
  case res of
    Just (Nothing, Nothing) -> fail "Invalid slice"
    Just (Just e1, Nothing) -> return (EPostfix PostfixSlice e1)
    Just (Nothing, Just e2) -> return (EPrefix PrefixSlice e2)
    Just (Just e1, Just e2) -> return (EBinary BinarySlice e1 e2)
    Nothing -> eTerm

transformSlice :: Expression -> Expression -> Expression
transformSlice (ELocated e p) e1 = ELocated (transformSlice e e1) p
transformSlice (EBinary BinarySlice e1 e2) e3 =
  EApplication (EProperty "slice" e3) [e1, e2]
transformSlice (EPostfix PostfixSlice e1) e2 =
  EApplication (EProperty "slice" e2) [e1, len]
 where
  len = EApplication (EProperty "len" e2) []
transformSlice (EPrefix PrefixSlice e2) e1 =
  EApplication (EProperty "slice" e1) [ELiteral (LInt 0), e2]
transformSlice e1 e2 = EListIndex e2 e1

tRequire :: Parser [Expression]
tRequire = do
  _ <- reserved "require"
  (: []) . ERequire <$> stringLiteral

tCustomOperator :: Parser [Expression]
tCustomOperator = do
  opTy <-
    choice
      [ reserved "infixl" $> CInfixL
      , reserved "infixr" $> CInfixR
      , reserved "infix" $> CInfixN
      , reserved "prefix" $> CPrefix
      , reserved "postfix" $> CPostfix
      ]
  prec <- option 0 $ fromInteger <$> integer
  name <- some operator
  let op = map (\n -> CustomOperator n prec opTy) name
  modifyIORef' customOperators (op <>)
  mempty

functionCallOperator :: Parser (Expression -> Expression)
functionCallOperator = do
  arguments <- do
    _ <- symbol "("
    ilevel <- ask

    -- Parsing function call args either inlined or indented
    args <-
      indentSepBy eExpression comma
        <|> (eExpression `sepBy` comma)

    _ <-
      indentSameOrInline ilevel (symbol ")")
        <|> indentSameOrHigher ilevel (symbol ")")

    return args

  -- Optional syntaxic sugar for callback argument
  lambdaArg <- optional $ do
    _ <- symbol "=>"
    EClosure [] Nothing <$> indentOrInline

  let newArgs = arguments ++ maybeToList lambdaArg

  return $ flip EApplication newArgs

parseToplevel :: Parser [Expression]
parseToplevel =
  eLocatedMany
    ( choice
        [ tCustomOperator
        , tRequire
        , eType
        , try eNativeGroup
        , eNativeFunction
        , eExtension
        , eMacroFunction
        , eMacro
        , (: []) <$> parseStatement
        ]
    )

parseProgram :: Parser Program
parseProgram =
  concat . catMaybes
    <$> many (nonIndented parseToplevel <* optional indentSc)
