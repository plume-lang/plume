module Plume.Syntax.Parser.Parser where

import Control.Monad.Combinators.Expr qualified as P
import Control.Monad.Parser qualified as P
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

import Plume.Syntax.Common qualified as Cmm
import Plume.Syntax.Concrete qualified as CST
import Plume.Syntax.Parser.Lexer qualified as L

import Plume.Syntax.Parser.Modules.Literal qualified as Lit
import Plume.Syntax.Parser.Modules.Operator qualified as Opr
import Plume.Syntax.Parser.Modules.Pattern qualified as Pat
import Plume.Syntax.Parser.Modules.Slice qualified as Slc
import Plume.Syntax.Parser.Modules.Type qualified as Typ

type Argument = Cmm.Annotation (Maybe Cmm.PlumeType, CST.IsMutable)

-- SOME UTILITY FUNCTIONS

-- | Parses a function call postfix operator
functionCallOperator :: P.Parser (CST.Expression -> CST.Expression)
functionCallOperator = do
  arguments <- L.parens (parseExpression `P.sepBy` L.comma)

  -- Optional syntaxic sugar for callback argument
  lambdaArg <- optional $ do
    _ <- L.symbol "=>"
    CST.EClosure [] Nothing <$> eBlock

  let newArgs = arguments ++ maybeToList lambdaArg

  return $ flip CST.EApplication newArgs

-- | Parses an expression with its position
eLocated :: P.Parser CST.Expression -> P.Parser CST.Expression
eLocated p = do
  p1 <- P.getSourcePos
  e <- p
  p2 <- P.getSourcePos
  return $ e CST.:>: (p1, p2)

eLocatedMany :: P.Parser [CST.Expression] -> P.Parser [CST.Expression]
eLocatedMany p = do
  p1 <- P.getSourcePos
  es <- p
  p2 <- P.getSourcePos
  return $ map (CST.:>: (p1, p2)) es

-- | Parses a mutable argument (for instance closure arguments)
mutArg :: P.Parser Argument
mutArg =
  P.option False (True <$ L.reserved "mut") >>= \mut -> do
    name <- L.identifier
    typ <- P.optional $ L.symbol ":" *> Typ.tType
    return $ name Cmm.:@: (typ, mut)

-- | Parses an annotated parser with a generic annotation
annotated :: P.Parser a -> P.Parser (Cmm.Annotation (Maybe a))
annotated p = do
  n <- L.identifier
  a <- optional $ L.symbol ":" *> p
  return $ n Cmm.:@: a

-- | Parses a type annotation, useful to parse variable declarations
typeAnnot :: P.Parser (Cmm.Annotation (Maybe Cmm.PlumeType))
typeAnnot = annotated Typ.tType

typeAnnot' :: P.Parser Cmm.PlumeType
typeAnnot' = L.identifier *> L.symbol ":" *> Typ.tType

eBlock :: P.Parser CST.Expression
eBlock =
  P.choice
    [ eLocated $ CST.EBlock <$> L.braces (P.many parseStatement)
    , parseExpression
    ]

-- EXPRESSION PARSERS

eVariable :: P.Parser CST.Expression
eVariable = CST.EVariable <$> L.identifier

eLiteral :: P.Parser CST.Expression
eLiteral = Lit.parseLiteral parseExpression

eIf :: P.Parser CST.Expression
eIf = do
  void $ L.reserved "if"
  cond <- parseExpression
  thenBlock <- eBlock
  elseBlock <- P.optional $ do
    void $ L.reserved "else"
    eBlock

  return $ CST.EConditionBranch cond thenBlock elseBlock

eSwitch :: P.Parser CST.Expression
eSwitch = do
  void $ L.reserved "switch"
  cond <- parseExpression
  cases <- L.braces . P.many $ do
    void $ L.reserved "case"
    val <- Pat.parsePattern
    block <- L.symbol "=>" *> parseExpression <|> eBlock
    return (val, block)

  return $ CST.ESwitch cond cases

eList :: P.Parser CST.Expression
eList =
  CST.EList <$> L.brackets (parseExpression `P.sepBy` L.comma)

eMacroExpr :: P.Parser CST.Expression
eMacroExpr =
  P.choice
    [ P.try parseMacroCall
    , parseMacroVar
    ]
 where
  parseMacroVar :: P.Parser CST.Expression
  parseMacroVar = do
    CST.EMacroVariable
      <$> (P.char '@' *> L.identifier)

  parseMacroCall :: P.Parser CST.Expression
  parseMacroCall = do
    name <- P.char '@' *> L.identifier
    args <- L.parens $ parseExpression `P.sepBy` L.comma
    return $ CST.EMacroApplication name args

eClosure :: P.Parser CST.Expression
eClosure = do
  void $ L.reserved "fn"
  args <- L.parens $ mutArg `P.sepBy` L.comma
  retTy <- P.optional $ L.symbol ":" *> Typ.tType
  void $ L.symbol "=>"

  CST.EClosure args retTy <$> eBlock

eTuple :: P.Parser CST.Expression
eTuple = buildTuple <$> L.parens (parseExpression `P.sepBy1` L.comma)
 where
  buildTuple [] = CST.EVariable "unit"
  buildTuple [x] = x
  buildTuple (x : xs) = CST.EApplication (CST.EVariable "tuple") [x, buildTuple xs]

parseTerm :: P.Parser CST.Expression
parseTerm =
  P.choice
    [ eLiteral
    , eIf
    , eSwitch
    , eVariable
    , eList
    , eMacroExpr
    , eClosure
    , eTuple
    , CST.EBlock <$> L.braces (P.many parseStatement)
    , L.parens parseExpression
    ]

parseExpression :: P.Parser CST.Expression
parseExpression = eLocated $ do
  customOps <- readIORef L.customOperators
  let customs = Opr.sortCustomOperators customOps
  P.makeExprParser parseTerm ([postfixOperators] : customs ++ Opr.operators)
 where
  postfixOperators =
    P.Postfix $
      Opr.makeUnaryOp $
        P.choice
          [ functionCallOperator
          , Slc.transformSlice <$> L.brackets (Slc.parseSlice parseTerm)
          , -- Record selection e.x where e may be a record and x a label to
            -- select from the record
            CST.EProperty <$> (P.char '.' *> L.field <* L.scn)
          ]

-- STATEMENT PARSERS

sReturn :: P.Parser CST.Expression
sReturn = do
  void $ L.reserved "return"
  CST.EReturn <$> eBlock

sMutDeclaration :: P.Parser CST.Expression
sMutDeclaration = do
  void $ L.reserved "mut"
  name <- typeAnnot
  void $ L.symbol "="
  value <- eBlock

  body <- P.optional $ L.reserved "in" *> eBlock

  return $ CST.EDeclaration [] True name value body

sDeclaration :: P.Parser CST.Expression
sDeclaration = do
  name <- P.try $ typeAnnot <* L.symbol "="
  value <- eBlock

  body <- P.optional $ L.reserved "in" *> eBlock

  return $ CST.EDeclaration [] False name value body

sFunction :: P.Parser CST.Expression
sFunction = do
  void $ L.reserved "fn"
  name <- L.identifier
  args <- L.parens $ mutArg `P.sepBy` L.comma
  retTy <- P.optional $ L.symbol ":" *> Typ.tType
  body <- L.symbol "=>" *> parseExpression <|> eBlock

  let cl = CST.EClosure args retTy body

  return $
    CST.EDeclaration
      []
      False
      (name Cmm.:@: Nothing)
      cl
      Nothing

parseStatement :: P.Parser CST.Expression
parseStatement =
  eLocated $
    P.choice
      [ sMutDeclaration
      , sDeclaration
      , sFunction
      , sReturn
      , parseExpression
      ]

-- EXTENSION PARSERS

eExtensionMember :: P.Parser (CST.ExtensionMember Cmm.PlumeType)
eExtensionMember =
  P.choice
    [ eExtFunction
    ]

eExtFunction :: P.Parser (CST.ExtensionMember Cmm.PlumeType)
eExtFunction = do
  void $ L.reserved "fn"
  name <- L.identifier <|> L.parens L.operator
  gens <- P.option [] $ L.angles (Typ.parseGeneric `P.sepBy` L.comma)
  args <- L.parens (mutArg `P.sepBy` L.comma)
  ret <- optional (L.symbol ":" *> Typ.tType)
  body <- L.symbol "=>" *> parseExpression <|> eBlock
  pure $
    CST.ExtDeclaration
      gens
      (name Cmm.:@: Nothing)
      (CST.EClosure args ret body)

-- TOP-LEVEL DECLARATIONS

tRequire :: P.Parser [CST.Expression]
tRequire = do
  void $ L.reserved "require"
  (:[]) . CST.ERequire <$> Lit.stringLiteral

tNative :: P.Parser [CST.Expression]
tNative = do
  void $ L.reserved "native"
  path <- Lit.stringLiteral <* P.notFollowedBy (L.reserved "with")
  name <- L.identifier
  gens <- P.option [] $ L.angles $ L.identifier `P.sepBy` L.comma
  args <- L.parens $ typeAnnot' `P.sepBy` L.comma
  retTy <- P.option Cmm.TUnit $ L.symbol ":" *> Typ.tType

  let clTy = args Cmm.:->: retTy

  return [CST.ENativeFunction path name gens clTy]

tNativeGroup :: P.Parser [CST.Expression]
tNativeGroup = do
  void $ L.reserved "native"
  path <- Lit.stringLiteral

  L.braces . P.many $ parseNative path
 where
  parseNative :: Text -> P.Parser CST.Expression
  parseNative p = do
    name <- L.identifier
    gens <- P.option [] $ L.angles $ L.identifier `P.sepBy` L.comma
    args <- L.parens $ typeAnnot' `P.sepBy` L.comma
    retTy <- P.option Cmm.TUnit $ L.symbol ":" *> Typ.tType

    let clTy = args Cmm.:->: retTy

    return $ CST.ENativeFunction p name gens clTy

tCustomOperator :: P.Parser [CST.Expression]
tCustomOperator = do
  opTy <-
    P.choice
      [ L.reserved "infixl" $> L.CInfixL
      , L.reserved "infixr" $> L.CInfixR
      , L.reserved "infix" $> L.CInfixN
      , L.reserved "prefix" $> L.CPrefix
      , L.reserved "postfix" $> L.CPostfix
      ]
  prec <- P.option 0 $ fromInteger <$> Lit.integer
  name <- some L.operator
  let op = map (\n -> L.CustomOperator n prec opTy) name
  modifyIORef' L.customOperators (op <>)
  mempty

tExtension :: P.Parser [CST.Expression]
tExtension = do
  void $ L.reserved "extend"
  gens <- P.option [] $ L.angles (Typ.parseGeneric `P.sepBy` L.comma)
  ty <- L.parens ((Cmm.:@:) <$> L.identifier <*> (L.symbol ":" *> Typ.tType))
  members <- L.braces (P.many eExtensionMember)
  return [CST.ETypeExtension gens ty members]

tType :: P.Parser [CST.Expression]
tType = do
  void $ L.reserved "type"
  name <- L.identifier
  gens <- P.option [] $ L.angles (Typ.parseGeneric `P.sepBy` L.comma)
  cons <- L.braces (Typ.typeConstructor `P.sepBy` L.comma)
  return [CST.EType (Cmm.Annotation name gens) cons]

tMacro :: P.Parser [CST.Expression]
tMacro = do
  void $ L.reserved "macro"
  name <- L.identifier <* L.symbol "="
  body <- eBlock
  return [CST.EMacro name body]

tMacroFunction :: P.Parser [CST.Expression]
tMacroFunction = do
  void $ L.reserved "macro"
  name <- L.identifier
  args <- L.parens $ L.identifier `P.sepBy` L.comma
  body <- L.symbol "=>" *> parseExpression <|> eBlock
  return [CST.EMacroFunction name args body]

parseToplevel :: P.Parser [CST.Expression]
parseToplevel =
  eLocatedMany $
    P.choice
      [ P.try tNative
      , tNativeGroup
      , tRequire
      , tType
      , tCustomOperator
      , tExtension
      , P.try tMacro
      , tMacroFunction
      , pure <$> parseStatement
      ]

parseProgram :: P.Parser [CST.Expression]
parseProgram = concat <$> P.sepEndBy parseToplevel L.scn