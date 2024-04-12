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

data OperatorType
  = CInfixL
  | CInfixR
  | CInfixN
  | CPrefix
  | CPostfix
  deriving (Show, Eq, Ord)

data CustomOperator
  = CustomOperator
  { customOperator :: Text
  , precedence :: Int
  , opType :: OperatorType
  }
  deriving (Show)

instance Eq CustomOperator where
  x == y = customOperator x == customOperator y && opType x == opType y

instance Ord CustomOperator where
  compare CustomOperator {precedence = p1, opType = t1, customOperator = o1} CustomOperator {precedence = p2, opType = t2, customOperator = o2} 
    | p1 == p2 = compare (t1, o1) (t2, o2)
    | otherwise = compare p1 p2

{-# NOINLINE operatorsCombinators #-}
operatorsCombinators :: IORef [[Operator Parser Expression]]
operatorsCombinators = unsafePerformIO $ newIORef []

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

multilineComment :: Parser ()
multilineComment = L.skipBlockComment "/*" "*/"

scn :: Parser ()
scn = L.space space1 lineComment multilineComment

isReal :: (Num a, RealFrac a) => a -> Bool
isReal x = (ceiling x :: Integer) == floor x

reservedWords :: Set Text
reservedWords =
  Set.fromList
    [ "in"
    , "if"
    , "then"
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
    , "with"
    , "extends"
    , "operator"
    , "type"
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
    ]

customOperators :: IORef (SL.SortedList CustomOperator)
{-# NOINLINE customOperators #-}
customOperators = unsafePerformIO $ newIORef mempty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

symbol :: Text -> Parser Text
symbol = lexeme . L.symbol scn

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

reservedOperators :: Set Text
reservedOperators =
  Set.fromList
    [ "->"
    , ":"
    , "."
    , ".."
    , "=>"
    ]

operator :: Parser Text
operator = do
  op <- lexeme $ takeWhile1P Nothing (`Set.member` validOperators)
  guard (op `Set.notMember` reservedOperators)
  return op

reserved :: Text -> Parser Text
reserved keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

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

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

isIdentCharStart :: Text -> Bool
isIdentCharStart cs = isAlpha (Text.head cs) || Text.head cs == '_'

nonLexedID :: Parser Text
nonLexedID = do
  r <- takeWhile1P Nothing isIdentChar
  -- Guarding parsed result and failing when reserved word is parsed
  -- (such as reserved keyword)
  if r `Set.member` reservedWords
    then fail $ "The identifier " ++ show r ++ " is a reserved word"
    else return r

identifier :: Parser Text
identifier = lexeme $ do
  cs <- takeWhile1P Nothing isIdentChar
  if cs `Set.member` reservedWords
    then fail $ "The identifier " ++ show cs ++ " is a reserved word"
    else
      if isIdentCharStart cs
        then return cs
        else fail $ "The identifier " ++ show cs ++ " is not valid"

field :: Parser Text
field = nonLexedID <|> operator
