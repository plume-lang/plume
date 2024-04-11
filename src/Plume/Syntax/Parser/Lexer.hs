{-# LANGUAGE DuplicateRecordFields #-}

module Plume.Syntax.Parser.Lexer where

import Control.Monad.Parser
import Data.Char
import Data.Text (pack)
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
  deriving (Show, Eq)

data CustomOperator
  = CustomOperator
  { customOperator :: Text
  , precedence :: Int
  , opType :: OperatorType
  }
  deriving (Show, Eq)

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

multilineComment :: Parser ()
multilineComment = L.skipBlockComment "/*" "*/"

scn :: Parser ()
scn = L.space space1 lineComment multilineComment

isReal :: (Num a, RealFrac a) => a -> Bool
isReal x = (ceiling x :: Integer) == floor x

reservedWords :: [Text]
reservedWords =
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

-- Tab width for the indent sensitive parser
-- Defaulting to Nothing, meaning that the tab width is not set
-- It is set on first space or tab consumption
tabWidthRef :: IORef (Maybe Int)
{-# NOINLINE tabWidthRef #-}
tabWidthRef = unsafePerformIO $ newIORef Nothing

-- Tab utility to tell the parser whether to use tabs or spaces
isTabIndent :: IORef (Maybe Bool)
{-# NOINLINE isTabIndent #-}
isTabIndent = unsafePerformIO $ newIORef Nothing

customOperators :: IORef [CustomOperator]
{-# NOINLINE customOperators #-}
customOperators = unsafePerformIO $ newIORef []

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

symbol :: Text -> Parser Text
symbol = lexeme . L.symbol scn

validOperators :: [Char]
validOperators =
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

reservedOperators :: [Text]
reservedOperators =
  [ "->"
  , ":"
  , "."
  , ".."
  , "=>"
  ]

operator :: Parser Text
operator = do
  op <-
    pack
      <$> lexeme
        ( (:) <$> oneOf validOperators <*> many (oneOf validOperators)
        )
  guard (op `notElem` reservedOperators)
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

identifierHelper :: Bool -> Parser Text
identifierHelper isLexed = do
  let lex = if isLexed then lexeme else id
  r <-
    lex
      ( pack
          <$> ( (:)
                  <$> (letterChar <|> oneOf ("_" :: String))
                  <*> many (alphaNumChar <|> oneOf ("_" :: String))
              )
      )

  -- Guarding parsed result and failing when reserved word is parsed
  -- (such as reserved keyword)
  if r `elem` reservedWords
    then fail $ "The identifier " ++ show r ++ " is a reserved word"
    else return r

identifier :: Parser Text
identifier = identifierHelper True

field :: Parser Text
field = identifierHelper False <|> operator
