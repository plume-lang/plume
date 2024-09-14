{-# LANGUAGE DuplicateRecordFields #-}

module Plume.Syntax.Parser.Lexer where

import Control.Monad.Combinators.Expr
import Control.Monad.Parser
import Data.Char
import Data.Set qualified as Set
import Data.SortedList qualified as SL
import Data.Text qualified as Text
import Plume.Syntax.Concrete
import System.IO.Unsafe
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (modify)

-- | Operator type
-- | CInfixL: Left-associative infix operator
-- | CInfixR: Right-associative infix operator
-- | CInfixN: Non-associative infix operator
-- | CPrefix: Prefix operator
-- | CPostfix: Postfix operator
-- |
-- | An infix operator is just a binary operator
data OperatorType
  = CInfixL
  | CInfixR
  | CInfixN
  | CPrefix
  | CPostfix
  deriving (Show, Eq, Ord)

-- | Custom operator
-- | Custom operator holds the operator, precedence and the type of the operator
-- | The precedence is used to determine the order the operator is parsed
-- | The type is used to determine the associativity of the operator
-- |  - Associativity describe how we should parse for instance a + b + c
-- |    * Left-associative: a + b + c = (a + b) + c
-- |    * Right-associative: a + b + c = a + (b + c)
-- |    * Non-associative: a + b + c = error
data CustomOperator
  = CustomOperator
  { customOperator :: Text
  , precedence :: Int
  , opType :: OperatorType
  }
  deriving (Show)

{-# NOINLINE defaultPosition #-}
defaultPosition :: IORef (Maybe Position)
defaultPosition = unsafePerformIO $ newIORef Nothing

{-# NOINLINE orphanExtCounter #-}
orphanExtCounter :: IORef Int
orphanExtCounter = unsafePerformIO $ newIORef 0

-- OPERATOR INSTANCES

instance Eq CustomOperator where
  x == y = customOperator x == customOperator y && opType x == opType y

-- | Custom operator is ordered by precedence and then by operator type
-- | The precedence is primarly used to determine the order the operator is parsed
-- | But if two operators have the same precedence, the operator type and name 
-- | are used to determine the order.
instance Ord CustomOperator where
  compare 
    CustomOperator {precedence = p1, opType = t1, customOperator = o1} 
    CustomOperator {precedence = p2, opType = t2, customOperator = o2} 
      | p1 == p2 = compare (t1, o1) (t2, o2)
      | otherwise = compare p1 p2

-- MUTABLE CUSTOM OPERATORS STATE

{-# NOINLINE operatorsCombinators #-}
operatorsCombinators :: IORef [[Operator Parser Expression]]
operatorsCombinators = unsafePerformIO $ newIORef []

{-# NOINLINE customOperators #-}
customOperators :: IORef (SL.SortedList CustomOperator)
customOperators = unsafePerformIO $ newIORef mempty

-- LEXING FUNCTIONS

-- | Skip inline comments
-- | Inline comments are comments that start with // and end at the end 
-- | of the line.
lineComment :: Parser ()
lineComment = L.skipLineComment "//"

-- | Skip block comments
-- | Block comments are comments that start with /* and end with */
-- | Block comments can span multiple lines
-- | Block comments are also nestable
multilineComment :: Parser ()
multilineComment = L.skipBlockComment "/*" "*/"

-- | Describe how to treat whitespace and comments
scn :: Parser ()
scn = L.space space1 lineComment multilineComment

-- | Describe the reserved keywords and primitive of the language
-- | It is used to tell the identifier parser what is a reserved keyword
-- | and what should an identifier not be.
reservedWords :: Set Text
reservedWords =
  Set.fromList
    [ "in"
    , "if"
    , "else"
    , "true"
    , "false"
    , "require"
    , "switch"
    , "fn"
    , "case"
    , "return"
    , "extend"
    , "native"
    , "type"
    , "declare"
    , "extends"
    , "interface"
    , "await"
    , "pub"
    , "with"
    , -- Primitive types
      "int"
    , "str"
    , "char"
    , "float"
    , "bool"
    , "infix"
    , "prefix"
    , "postfix"
    , "infixl"
    , "infixr"
    , "mut"
    , "while"
    ]

-- | Lexeme parser that consumes whitespace after the lexeme
-- | This is an utility function that converts a non-lexeme parser
-- | into a lexeme parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

-- | Parse a symbol and consume whitespace after the symbol
symbol :: Text -> Parser Text
symbol = lexeme . L.symbol scn

-- | Describe the valid operators in the language
-- | Any other operator is ovbiously invalid
-- | This may be used with `some` to create an operator composed
-- | of valid operators, such as `<+>`, `<!>`, `<?>`
validOperators :: Set Char
validOperators =
  Set.fromList
    [ '!'
    , '#'
    , '$'
    , '%'
    , '&'
    , '*'
    , '+'
    , '.'
    , '/'
    , '<'
    , '='
    , '>'
    , '?'
    , '@'
    , '^'
    , '|'
    , '-'
    , '~'
    ]

-- | Describe the reserved operators in the language
-- | Reserved operators are operators that are used in the language
-- | and cannot be used as custom operators
-- | This is used to prevent the user from defining operators that
-- | may break the parser (for instance `=>` is used in lambda expressions)
reservedOperators :: Set Text
reservedOperators =
  Set.fromList
    [ "->"
    , ":"
    , "."
    , ".."
    , "=>"
    ]

-- | Parse a sequence of valid operators, checking if they are valid
-- | and not reserved, and then return the concatenated operator
operator :: Parser Text
operator = do
  op <- lexeme $ takeWhile1P Nothing (`Set.member` validOperators)
  guard (op `Set.notMember` reservedOperators)
  return op

-- | Parse a reserved keyword
-- | A reserved keyword is a keyword that is used in the language
-- | and cannot be used as an identifier. For instance `if`, `else`, `return`
-- | are reserved keywords. It should not be followed by an alphanumeric
-- | character: `if` is a reserved keyword, but `iff` is not.
reserved :: Text -> Parser Text
reserved keyword = try $ lexeme (string keyword <* notFollowedBy alphaNumChar)

-- BASIC LEXEME PARSERS

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = symbol ":"

-- | Check if the character is a valid identifier character
-- | An identifier character is an alphanumeric character or an underscore
-- | For instance, `'test` is not a valid identifier, but `test` is. 
isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

-- | Check if the character is a valid identifier start character
-- | An identifier start character is an alphabetic character or an underscore
-- | For instance, `'test` is not a valid identifier, but `test` is.
-- | `1` is not a valid identifier too.
isIdentCharStart :: Text -> Bool
isIdentCharStart cs = isAlpha (Text.head cs) || Text.head cs == '_'

-- | Parse a non-lexed identifier
-- | A non-lexed identifier is an identifier that is not lexed, meaning that
-- | it does not consume whitespace after and before the identifier. This 
-- | is useful for parsing record selections.
nonLexedID :: Parser Text
nonLexedID = do
  r <- Text.intercalate "::" <$> (takeWhile1P Nothing isIdentChar `sepBy1` string "::")
  -- Guarding parsed result and failing when reserved word is parsed
  -- (such as reserved keyword)
  if r `Set.member` reservedWords
    then fail $ "The identifier " ++ show r ++ " is a reserved word"
    else
      if isIdentCharStart r
        then return r
        else fail $ "The identifier " ++ show r ++ " is not valid"

-- | Parse an identifier
-- | An identifier is a sequence of valid identifier characters
-- | that starts with an identifier start character
-- | An identifier cannot be a reserved keyword
identifier :: Parser Text
identifier = lexeme $ do
  cs <- Text.intercalate "::" <$> (takeWhile1P Nothing isIdentChar `sepBy1` string "::")
  if cs `Set.member` reservedWords
    then fail $ "The identifier " ++ show cs ++ " is a reserved word"
    else
      if isIdentCharStart cs
        then return cs
        else fail $ "The identifier " ++ show cs ++ " is not valid"

-- | A field may be either a non-lexed identifier or an operator
field :: Parser Text
field = nonLexedID <|> operator

-- | Check if the given number is an integer, meaning that
-- | the number is equal to its floor and ceiling
-- | For instance 1.0 is an integer because floor(1.0) = 1 = ceiling(1.0) 
-- | But 1.6 is not an integer because floor(1.6) = 1 != ceiling(1.6)
isInteger :: (Num a, RealFrac a) => a -> Bool
isInteger x = (ceiling x :: Integer) == floor x