module Plume.Syntax.Parser.Parser where

import Control.Monad.Combinators.Expr qualified as P
import Control.Monad.Parser qualified as P
import Data.SortedList qualified as SL
import Plume.Syntax.Common qualified as Cmm
import Plume.Syntax.Concrete qualified as CST
import Plume.Syntax.Parser.Lexer qualified as L
import Plume.Syntax.Parser.Modules.Literal qualified as Lit
import Plume.Syntax.Parser.Modules.Operator qualified as Opr
import Plume.Syntax.Parser.Modules.Pattern qualified as Pat
import Plume.Syntax.Parser.Modules.Slice qualified as Slc
import Plume.Syntax.Parser.Modules.Type qualified as Typ
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

type Argument = Cmm.Annotation (Maybe Cmm.PlumeType, CST.IsMutable)

-- SOME UTILITY FUNCTIONS

-- | Parses a function call postfix operator
-- | This is used to parse function calls in the form of `f(x, y, z)`
-- | This is a postfix operator because it is applied after the expression
-- | is parsed
-- |
-- | Using a postfix operator to model function calls lets us writing 
-- | calls in some specific forms, such as:
-- |
-- | - f(x)(y)
-- | - f(x, y)
-- | - f(x)(y, z)
-- | - f(x)(y)(z)
-- | - (f(x))(y)
functionCallOperator :: P.Parser (CST.Expression -> CST.Expression)
functionCallOperator = do
  arguments <- L.parens (parseExpression `P.sepBy` L.comma)

  return $ flip CST.EApplication arguments

-- | Parses an expression with its position
-- | Position is a tuple of two source positions, the first one is the
-- | beginning of the expression and the second one is the end of the
-- | expression. 
-- | They're stored in a `Located` annotation, which is a tuple of the
-- | expression and the position.
eLocated :: P.Parser CST.Expression -> P.Parser CST.Expression
eLocated p = do
  p1 <- P.getSourcePos
  e <- p
  p2 <- P.getSourcePos
  return $ e CST.:>: (p1, p2)

-- | Parses a list of expressions with a global position
-- | This is useful to parse a list of expressions from a single expression
-- | in the source code. This way, we can get the position of the whole
-- | expression.
-- | This is generally used to parse toplevel expressions as they may return 
-- | multiple expressions.
eLocatedMany :: P.Parser [CST.Expression] -> P.Parser [CST.Expression]
eLocatedMany p = do
  p1 <- P.getSourcePos
  es <- p
  p2 <- P.getSourcePos
  return $ map (CST.:>: (p1, p2)) es

-- | Parses a mutable argument (for instance closure arguments)
-- | A mutable argument is a closure argument generally that follow
-- | this syntax: `mut name: Type` where `mut` is a keyword that
-- | indicates that the argument is mutable.
-- | Mut keyword is optional, so the argument may be immutable.
mutArg :: P.Parser Argument
mutArg =
  P.option False (True <$ L.reserved "mut") >>= \mut -> do
    name <- L.identifier
    typ <- P.optional $ L.symbol ":" *> Typ.tType
    return $ name Cmm.:@: (typ, mut)

-- | Parses an annotated parser with a generic annotation
-- | This is useful to parse generic annotations in declarations
-- | or type annotations.
-- | The annotation is a name followed by a colon and the parser
-- | result, of the form `name: parser?` where `parser` is generally
-- | a type parser.
annotated :: P.Parser a -> P.Parser (Cmm.Annotation (Maybe a))
annotated p = do
  n <- L.lexeme $ L.nonLexedID <* P.lookAhead (P.optional $ L.symbol ":")
  a <- optional $ L.symbol ":" *> p
  return $ n Cmm.:@: a

-- | Parses a type annotation, useful to parse variable declarations.
-- | It is a direct application of the `annotated` function with a type
-- | parser.
typeAnnot :: P.Parser (Cmm.Annotation (Maybe Cmm.PlumeType))
typeAnnot = annotated Typ.tType

-- | Parses a type annotation, but without optional type parsing 
-- | This is useful to parse type extension's type
typeAnnot' :: P.Parser Cmm.PlumeType
typeAnnot' = L.identifier *> L.symbol ":" *> Typ.tType

-- | Parses either a block of expressions or a single expression
-- | This is a generaly expression parser used to modelize function
-- | bodies, if branches, else branches, variable declarations, etc.
-- | 
-- | SYNTAX: 
-- |  - { expr1; expr2; expr3; ... }
-- |  - expr
eBlock :: P.Parser CST.Expression
eBlock =
  P.choice
    [ eLocated $ CST.EBlock <$> L.braces (P.many parseStatement)
    , parseExpression
    ]

-- EXPRESSION PARSERS

-- | Parses a variable expression
-- | A variable expression is a simple expression that is just an identifier
-- | 
-- | SYNTAX:
-- |  - identifier that starts with a letter or underscore and may
-- |    contain letters, digits and underscores
-- |  - it should not be a keyword
-- |  - it should not be followed by a colon or an equal sign (avoid 
-- |    declaration conflicts)
eVariable :: P.Parser CST.Expression
eVariable = CST.EVariable <$> L.lexeme (L.nonLexedID <* isNotDecl)
  where 
    isNotDecl :: P.Parser ()
    isNotDecl = P.notFollowedBy (L.lexeme $ P.oneOf (":=" :: String))

-- | Parses an if expression
-- | An if expression is a conditional expression that may have an else
-- | branch.
-- |
-- | SYNTAX:
-- |  - if condition { thenBlock } else { elseBlock }
-- |  - if condition { thenBlock }
-- | where `condition` is an expression that returns a boolean value
-- | and `thenBlock` and `elseBlock` are blocks of expressions
eIf :: P.Parser CST.Expression
eIf = do
  void $ L.reserved "if"
  cond <- parseExpression
  thenBlock <- eBlock
  elseBlock <- P.optional $ do
    void $ L.reserved "else"
    eBlock

  return $ CST.EConditionBranch cond thenBlock elseBlock

-- | Parses a switch expression
-- | A switch expression is a conditional expression that may have multiple
-- | cases. It is the equivalent of a case-of expression in Haskell.
-- |
-- | SYNTAX:
-- |  - switch condition { case pattern => block; case pattern => block; ... }
-- | where `condition` is an expression that is matched against the patterns
-- | and `pattern` is a pattern that may be matched against the condition.
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

-- | Parses a list expression
-- | A list expression is a collection of expressions that are separated by
-- | commas and enclosed in brackets.
-- |
-- | SYNTAX:
-- |  - [expr1, expr2, expr3, ...]
eList :: P.Parser CST.Expression
eList =
  CST.EList <$> L.brackets (parseExpression `P.sepBy` L.comma)

-- | Parses a macro expression
-- | A macro expression is a special kind of expression that is used to
-- | reference a macro variable or a macro function.
-- | Both are prefixed by the `@` symbol.
-- |
-- | SYNTAX:
-- |  - @variable
-- |  - @function(arg1, arg2, ...)
-- | where `variable` and `function` are identifiers and `arg1`, `arg2`, ...
-- | are expressions.
eMacroExpr :: P.Parser CST.Expression
eMacroExpr =
  P.choice
    [ parseMacroCall
    , parseMacroVar
    ]
 where
  parseMacroVar :: P.Parser CST.Expression
  parseMacroVar = do
    CST.EMacroVariable
      <$> (P.char '@' *> L.identifier)

  parseMacroCall :: P.Parser CST.Expression
  parseMacroCall = do
    name <- P.try $ P.char '@' *> L.identifier <* L.symbol "("
    args <- parseExpression `P.sepBy` L.comma
    void $ L.symbol ")"
    return $ CST.EMacroApplication name args

-- | Parses a closure expression
-- | A closure expression is a function-like expression that is not
-- | bound to a name. It is used to create anonymous functions.
-- |
-- | SYNTAX:
-- |  - fn(arg1, arg2, ...): retTy => expr
-- |  - fn(arg1, arg2, ...): retTy { block }
-- | where `arg1`, `arg2`, ... are mutable arguments (represented by the
-- | `mutArg` parser), `retTy` is an optional return type, `expr` is an
-- | expression, and `block` is a block of expressions.
eClosure :: P.Parser CST.Expression
eClosure = do
  void $ L.reserved "fn"
  args <- L.parens $ mutArg `P.sepBy` L.comma
  retTy <- P.optional $ L.symbol ":" *> Typ.tType
  body <-
    CST.EBlock
      <$> L.braces (P.many parseStatement)
        <|> ( L.symbol "=>"
                *> parseExpression
            )

  return $ CST.EClosure args retTy body

-- | Parses a tuple expression
-- | A tuple expression is a collection of expressions that are separated by
-- | commas and enclosed in parentheses.
-- | A tuple may have at least one element not to be confused with a single
-- | expression that is just parenthesized.
eTuple :: P.Parser CST.Expression
eTuple = buildTuple <$> L.parens (parseExpression `P.sepBy1` L.comma)
 where
  buildTuple [] = CST.EVariable "unit"
  buildTuple [x] = x
  buildTuple (x : xs) = CST.EApplication (CST.EVariable "tuple") [x, buildTuple xs]

-- | Parses a term expression
-- | A term expression is a simple expression that does not include any
-- | operator or complex expression that implies operator precedence.
parseTerm :: P.Parser CST.Expression
parseTerm =
  P.choice
    [ Lit.parseLiteral
    , eIf
    , eSwitch
    , eList
    , eMacroExpr
    , eClosure
    , eTuple
    , eVariable
    , CST.EBlock <$> L.braces (P.many parseStatement)
    , L.parens parseExpression
    ]

-- | Parses an expression
-- | An expression is a complex expression that may include operators and
-- | terms. It is the main expression parser.
parseExpression :: P.Parser CST.Expression
parseExpression = eLocated $ do
  -- Custom operators are read from the custom operators reference
  -- They're sorted by precedence by default thanks to the `SortedList`
  -- data structure.
  customOps <- readIORef L.operatorsCombinators

  -- The expression parser is built using the `makeExprParser` function
  -- - Postfix operators are added to the expression parser to allow
  --   function calls, mutable property access, record selection and slice
  --   expressions.
  -- - Custom operators are added to the expression parser to allow custom
  --   operators to be used in expressions.
  -- - And finally default operators are added to the expression parser to
  --   allow default operators to be used in expressions.
  --   Default operators are defined in the `Operator` module.
  P.makeExprParser parseTerm ([postfixOperators] : customOps ++ Opr.operators)
 where
  postfixOperators =
    P.Postfix $
      Opr.makeUnaryOp $
        P.choice
          [ functionCallOperator
          , mutAccess
          , listIdx
          , recSelection
          ]
  
  -- | Parses a mutable property access
  -- | A mutable property access is a shortcut to access a property of an
  -- | mutable expression without the need of dereferencing it.
  -- |
  -- | SYNTAX:
  -- |  - expr->field <=> (*expr).field
  -- | where `expr` is an expression that returns a mutable value and `field`
  -- | is the field that is accessed.
  mutAccess = do
    f <- P.string "->" *> L.nonLexedID
    return $ CST.EProperty f . CST.EUnMut

  -- | Parses a list index
  -- | A list index is a postfix operator that is used to access an element
  -- | of a list.
  -- |
  -- | SYNTAX:
  -- |  - expr[slice] <=> expr.slice(start, end)
  -- |  - expr[idx]   <=> expr.get_index(idx) 
  -- | where `expr` is an expression that returns a list, `slice` is a slice
  -- | expression and `idx` is an expression that returns an integer.
  listIdx = Slc.transformSlice <$> L.brackets (Slc.parseSlice parseTerm)

  -- | Parses a record selection
  -- | A record selection is a postfix operator that is used to access a field
  -- | of a record.
  -- |
  -- | SYNTAX:
  -- |  - expr.field
  -- | where `expr` is an expression that returns a record and `field` is the
  -- | field that is accessed.
  recSelection = CST.EProperty <$> (P.char '.' *> L.field <* L.scn)

-- STATEMENT PARSERS

-- | Parses a return statement
-- | A return statement is a statement that is used to return a value from a
-- | function.
-- |
-- | SYNTAX:
-- |  - return expr
-- | where `expr` is an expression that is returned from the function.
-- | `expr` can also be a block of expressions.
sReturn :: P.Parser CST.Expression
sReturn = do
  void $ L.reserved "return"
  CST.EReturn <$> eBlock

-- | Parses a mutable declaration
-- | A mutable declaration is a statement that is used to declare a mutable
-- | variable.
-- |
-- | SYNTAX:
-- |  - mut name: Type = expr
-- |  - mut name: Type = expr in block
-- | where `name` is an identifier, `Type` is a type annotation, `expr` is an
-- | expression that initializes the variable and `block` is a block of
-- | expressions.
sMutDeclaration :: P.Parser CST.Expression
sMutDeclaration = do
  void $ L.reserved "mut"
  name <- typeAnnot
  void $ L.symbol "="
  value <- eBlock

  body <- P.optional $ L.reserved "in" *> eBlock

  return $ CST.EDeclaration [] True name value body

-- | Parses a declaration
-- | A declaration is a statement that is used to declare a immutable
-- | variable.
-- |
-- | SYNTAX:
-- |  - name: Type = expr
-- |  - name: Type = expr in block
-- | where `name` is an identifier, `Type` is a type annotation, `expr` is an
-- | expression that initializes the variable and `block` is a block of
-- | expressions.
sDeclaration :: P.Parser CST.Expression
sDeclaration = do
  name <- P.try $ typeAnnot <* P.lookAhead (L.symbol "=")
  void $ L.symbol "="
  value <- eBlock

  body <- P.optional $ L.reserved "in" *> eBlock

  return $ CST.EDeclaration [] False name value body

-- | Parses a function declaration
-- | A function declaration is a statement that is used to declare a function.
-- | It is a syntactic sugar for a declaration with a closure expression.
-- |
-- | SYNTAX:
-- |  - fn name(generics) (arg1, arg2, ...): retTy => expr
-- |  - fn name(generics) (arg1, arg2, ...): retTy { block }
-- | where `name` is an identifier, `generics` is a list of generic annotations,
-- | `arg1`, `arg2`, ... are mutable arguments, `retTy` is an optional return
-- | type, `expr` is an expression and `block` is a block of expressions.
sFunction :: P.Parser CST.Expression
sFunction = do
  void $ L.reserved "fn"
  name <- L.identifier
  generics <- P.option [] $ L.angles $ Typ.parseGeneric `P.sepBy` L.comma
  args <- L.parens $ mutArg `P.sepBy` L.comma
  retTy <- P.optional $ L.symbol ":" *> Typ.tType
  body <- L.symbol "=>" *> parseExpression <|> eBlock

  let cl = CST.EClosure args retTy body

  return $
    CST.EDeclaration
      generics
      False
      (name Cmm.:@: Nothing)
      cl
      Nothing

-- | Parses a statement
-- | A statement is an expression that can be used both in top-level scope
-- | and in local scope (inside a block).
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

-- | Parses an extension member
-- | An extension member is a member that is added to a type to extend its
-- | functionalities.
eExtensionMember :: P.Parser (CST.ExtensionMember Cmm.PlumeType)
eExtensionMember =
  P.choice
    [ eExtFunction
    , eExtVariable
    ]

-- | Parses an extension variable
-- | An extension variable is a variable that is added to a type to extend its
-- | functionalities. It is a syntactic sugar for a declaration.
-- |
-- | SYNTAX:
-- |  - name: ty = expr
-- | where `name` is an identifier, `ty` is a type annotation and `expr` is
-- | an expression.
eExtVariable :: P.Parser (CST.ExtensionMember Cmm.PlumeType)
eExtVariable = do
  name <- P.try $ typeAnnot <* P.lookAhead (L.symbol "=")
  void $ L.symbol "="
  CST.ExtDeclaration [] name <$> parseExpression

-- | Parses an extension function
-- | An extension function is a function that is added to a type to extend its
-- | functionalities. It is a syntactic sugar for a function declaration.
-- |
-- | SYNTAX:
-- |  - fn name<generics>(arg1, arg2, ...): retTy => expr
-- |  - fn name<generics>(arg1, arg2, ...): retTy { block }
-- | where `name` is an identifier, `generics` is a list of generic annotations,
-- | `arg1`, `arg2`, ... are mutable arguments, `retTy` is an optional return
-- | type, `expr` is an expression and `block` is a block of expressions.
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

-- | Parses a require statement
-- | A require statement is a statement that is used to import a module.
-- |
-- | SYNTAX:
-- |  - require "path/to/module"
-- | where `path/to/module` is a string literal that represents the path to
-- | the module.
tRequire :: P.Parser [CST.Expression]
tRequire = do
  void $ L.reserved "require"
  (: []) . CST.ERequire <$> Lit.stringLiteral

-- | Parses an interface declaration
-- | An interface declaration is a statement that is used to declare a 
-- | Haskell-like typeclass.
-- |
-- | SYNTAX:
-- |  - interface<generics> (name: ty) { 
-- |      fn name<generics>(arg1: ty, arg2: ty, ...): retTy
-- |      ... 
-- |    }
tInterface :: P.Parser [CST.Expression]
tInterface = do
  void $ L.reserved "interface"
  gens <- P.option [] $ L.angles $ Typ.parseGeneric `P.sepBy` L.comma
  annot <- Cmm.Annotation <$> L.identifier <*> L.angles (Typ.tType `P.sepBy` L.comma)
  members <- L.braces (P.many iFun)
  return [CST.EInterface annot gens members]
  where 
    iFun = do
      void $ L.reserved "fn"
      name <- L.identifier
      gens <- P.option [] $ L.angles $ Typ.parseGeneric `P.sepBy` L.comma
      args <- L.parens $ typeAnnot' `P.sepBy` L.comma
      retTy <- P.option Cmm.TUnit $ L.symbol ":" *> Typ.tType
      return $ Cmm.Annotation name (Cmm.MkScheme gens (args Cmm.:->: retTy))

-- | Parses a native statement
-- | A native statement is a statement that is used to import a native module.
-- | A native module is a module that is written in another language and is
-- | compiled to a shared library.
-- |
-- | SYNTAX:
-- |  - native "path/to/module" { native_decls }
-- |  - native "path/to/module" native_decl
-- | where `path/to/module` is a string literal that represents the path to
-- | the native module and `native_decls` are a list of native declarations.
-- | `native_decl` is a single native declaration.
-- |
-- | A native declaration is a declaration that is used to declare a native
-- | function.
-- |
-- | SYNTAX:
-- |  - name<generics>(arg1: Type, arg2: Type, ...): retTy
-- | where `name` is an identifier, `generics` is a list of generic annotations,
-- | `arg1`, `arg2`, ... are arguments with type annotations and `retTy` is
-- | a return type.
tNative :: P.Parser [CST.Expression]
tNative = do
  void $ L.reserved "native"
  path <- Lit.stringLiteral
  P.choice [nativeGroup path, nativeOne path]
  where
    nativeGroup p = L.braces (P.many (parseNative p))
    nativeOne p = (: []) <$> parseNative p

    parseNative p = do
      name <- L.identifier
      gens <- P.option [] $ L.angles $ L.identifier `P.sepBy` L.comma
      args <- L.parens $ typeAnnot' `P.sepBy` L.comma
      retTy <- P.option Cmm.TUnit $ L.symbol ":" *> Typ.tType

      let clTy = args Cmm.:->: retTy

      return $ CST.ENativeFunction p name gens clTy

-- | Parses a custom operator
-- | A custom operator is an operator that is defined by the user. It can be
-- | used in expressions to define custom operators.
-- |
-- | SYNTAX:
-- |  - infixl 10 +-
-- |  - infixr 10 +-
-- |  - infix 10 +-
-- |  - prefix 10 +-
-- |  - postfix 10 +-
-- | where `infixl`, `infixr`, `infix`, `prefix` and `postfix` are keywords
-- | that define the operator type, `10` is the precedence of the operator and
-- | `+-` is the operator itself.
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
  let op = SL.toSortedList $ map (\n -> L.CustomOperator n prec opTy) name
  op' <- readIORef L.customOperators
  writeIORef L.customOperators $ SL.union op op'
  let newOps = Opr.sortCustomOperators (op `SL.union` op')
  writeIORef L.operatorsCombinators newOps
  mempty

-- | Parses a type extension
-- | A type extension is a statement that is used to extend a type with new
-- | members.
-- |
-- | SYNTAX:
-- |  - extend<generics> (name: ty) { ext_members }
-- | where `generics` is a list of generic annotations, `name` is an identifier,
-- | `ty` is a type annotation and `ext_members` are a list of extension members.
tExtension :: P.Parser [CST.Expression]
tExtension = do
  void $ L.reserved "extend"
  gens <- P.option [] $ L.angles (Typ.parseGeneric `P.sepBy` L.comma)
  
  tc <- Cmm.Annotation <$> L.identifier <*> L.angles (Typ.tType `P.sepBy1` L.comma)
  
  var <- P.optional $ L.reserved "with" *> L.identifier
  members <- L.braces (P.many eExtensionMember)
  return [CST.ETypeExtension gens tc var members]

-- | Parses a type declaration
-- | A type declaration is a statement that is used to declare a new
-- | user defined type.
-- |
-- | SYNTAX:
-- |  - type name<generics> { type_members }
-- | where `name` is an identifier, `generics` is a list of generic annotations
-- | and `type_members` are a list of type members.
tType :: P.Parser [CST.Expression]
tType = do
  void $ L.reserved "type"
  name <- L.identifier
  gens <- P.option [] $ L.angles (Typ.parseGeneric `P.sepBy` L.comma)
  cons <- L.braces (Typ.typeConstructor `P.sepBy` L.comma)
  return [CST.EType (Cmm.Annotation name gens) cons]

-- | Parses a macro
-- | A macro is a statement that is used to define a macro variable or a
-- | macro function.
-- |
-- | SYNTAX:
-- |  - macro name = expr
-- |  - macro name = block
-- |  - macro name(args) => expr
-- |  - macro name(args) { block }
-- | where `name` is an identifier, `args` are a list of identifiers, `expr` is
-- | an expression and `block` is a block of expressions.
tMacro :: P.Parser [CST.Expression]
tMacro = do
  void $ L.reserved "macro"
  name <- L.identifier
  (args, body, isFun) <- P.choice [macroFun, macroVar]
  return
    [if isFun then CST.EMacroFunction name args body else CST.EMacro name body]
 where
  macroFun = do
    args <- L.parens $ L.identifier `P.sepBy` L.comma
    body <- L.symbol "=>" *> parseExpression <|> eBlock
    return (args, body, True)
  macroVar = do
    body <- L.symbol "=" *> parseExpression
    return ([], body, False)

-- | Parses a toplevel expression
-- | A toplevel expression is an expression that is at the top-level of the
-- | source code. It may be a statement, a require statement, a native
-- | statement, a custom operator, a type extension, a type declaration or a
-- | macro. 
-- | It returns a list of expressions because a toplevel expression may return
-- | multiple expressions or even nothing (that's the case for custom operators).
parseToplevel :: P.Parser [CST.Expression]
parseToplevel =
  eLocatedMany $
    P.choice
      [ tNative
      , tInterface
      , tRequire
      , tType
      , tCustomOperator
      , tExtension
      , tMacro
      , pure <$> parseStatement
      ]

-- | Parses a program
-- | A program is a list of toplevel expressions.
-- | We use the `scn` lexer combinator to skip whitespaces and comments.
-- | Additionally, we use the `sepEndBy` combinator to parse toplevel
-- | expressions separated by newlines.
parseProgram :: P.Parser [CST.Expression]
parseProgram = L.scn *> (concat <$> P.sepEndBy parseToplevel L.scn)
