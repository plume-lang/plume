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
  deriving (Show)

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

multilineComment :: Parser ()
multilineComment = L.skipBlockComment "/*" "*/"

indentSc :: Parser ()
indentSc =
  skipMany
    ( choice
        [ try $ space *> lineComment
        , try $ space *> multilineComment
        , void eol
        ]
    )

scn :: Parser ()
scn = L.space space1 lineComment multilineComment

sc :: Parser ()
sc =
  L.space
    (void (char ' ' <|> char '\t'))
    lineComment
    multilineComment

isReal :: (Num a, RealFrac a) => a -> Bool
isReal x = (ceiling x :: Integer) == floor x

consumeIndents :: Parser Int
consumeIndents = do
  tabWidth <- readIORef tabWidthRef
  isTab <- readIORef isTabIndent

  -- Optionally consuming spaces and tabs
  ilevel <-
    optional $
      ( ( do
            sp <- howMany1 (char ' ')

            -- If the number of spaces is divisible by tab width then it is a valid
            -- indentation and we can return the number of tabs. Otherwise, we return
            -- the nearest integer value of the division of spaces by tab width.
            case isTab of
              Just True ->
                fail "Code should not mix tabs and spaces for indentation"
              Just False ->
                case tabWidth of
                  Just tw -> do
                    if isReal (fromIntegral sp / fromIntegral tw :: Double)
                      then return $ sp `div` tw
                      else
                        fail $
                          "Indentation level mismatch, tab width should be equal to "
                            ++ show tw
                            ++ " but received "
                            ++ show sp
                  Nothing -> do
                    writeIORef tabWidthRef (Just sp)
                    return 1
              Nothing -> do
                writeIORef isTabIndent (Just False)
                writeIORef tabWidthRef (Just sp)
                return 1
        )
          <|> ( do
                  tabs <- howMany1 (char '\t')
                  case isTab of
                    Just True ->
                      return tabs
                    Just False ->
                      fail "Code should not mix tabs and spaces for indentation"
                    Nothing -> do
                      writeIORef isTabIndent (Just True)
                      writeIORef tabWidthRef (Just tabs)
                      return tabs
              )
      )
        <* try indentSc

  -- If the indentation is not present, we return 0, basically meaning that
  -- this line is not indented.
  let processedIndent = fromMaybe 0 ilevel

  return processedIndent

reservedWords :: [Text]
reservedWords =
  [ "in"
  , "if"
  , "then"
  , "else"
  , "true"
  , "false"
  , "except"
  , "require"
  , "switch"
  , "case"
  , "return"
  , "extend"
  , "native"
  , "property"
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
  ]

notSpace :: Parser ()
notSpace = void $ satisfy (/= ' ')

space' :: Parser ()
space' = void $ satisfy isSpace

emptyLine :: Parser ()
emptyLine = do
  sc <* (notFollowedBy notSpace <|> void eol)

emptyLineWithoutConsumingEOL :: Parser ()
emptyLineWithoutConsumingEOL = do
  sc <* (notFollowedBy notSpace <|> void (lookAhead eol))

emptyLineWithEOL :: Parser ()
emptyLineWithEOL = do
  sc
  x <- (Nothing <$ notFollowedBy notSpace) <|> Just <$> optional eol
  case x of
    Nothing -> void eol
    Just (Just _) -> return ()
    Just Nothing -> fail "Expected a newline"

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
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = lexeme . L.symbol sc

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
reserved keyword = do
  r <- lexeme (string keyword <* notFollowedBy alphaNumChar)
  -- Guarding parsed result here lets the parser building more security
  -- on top of the language definition.
  guard (r `elem` reservedWords)
  return r

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

-- How many times a parser can be applied. It returns the number of
-- times the parser was applied.
howMany :: Parser a -> Parser Int
howMany p = length <$> many p

-- howMany variant that requires at least one application of the parser
howMany1 :: Parser a -> Parser Int
howMany1 p = length <$> some p

-- Indent parser takes a parser and applies it only and only if the
-- indentation level is greater than the current indentation level.
-- It returns a list of parsed results with the same indentation level.
indent :: Parser a -> Parser [a]
indent p = do
  ilevel <- ask
  level <- eol >> optional (try emptyLineWithEOL) >> consumeIndents
  if level > ilevel
    then do
      x <- local (const level) p
      xs <- many $ indentSame level (p <* optional (try emptyLineWithoutConsumingEOL))
      return (x : xs)
    else return []

indentOne :: Parser a -> Parser a
indentOne p = do
  ilevel <- ask
  level <- eol >> optional (try emptyLineWithEOL) >> consumeIndents
  if level > ilevel
    then local (const level) p
    else
      fail $
        "Indentation level mismatch, expected "
          <> show ilevel
          <> " but received "
          <> show level

indentSepBy :: Parser a -> Parser b -> Parser [a]
indentSepBy p sep = do
  ilevel <- ask
  level <- eol >> optional (try emptyLineWithEOL) >> consumeIndents
  if level > ilevel
    then do
      x <- local (const level) p <* sep
      xs <- many $ indentSame level (p <* sep)
      end <- indentSame level p
      return (x : xs ++ [end])
    else return []

-- Indent parser that takes a parser and applies it only and only if the
-- indentation level is equal to the current indentation level.
-- If it's not, then it returns Nothing
indentSameOrNothing :: Int -> Parser a -> Parser (Maybe a)
indentSameOrNothing = (optional <$>) . indentSame

indentSameOrHigher :: Int -> Parser a -> Parser a
indentSameOrHigher ilevel p = do
  level <- eol *> optional (try emptyLineWithEOL) *> consumeIndents
  if level >= ilevel
    then local (const level) p
    else
      fail $
        "Indentation level mismatch, expected "
          ++ show ilevel
          ++ " but received "
          ++ show level

-- Indent parser that takes a parser and applies it only and only if the
-- indentation level is equal to the current indentation level or on the
-- same line
indentSameOrInline :: Int -> Parser a -> Parser a
indentSameOrInline ilevel p = indentSame ilevel p <|> p

-- Indent parser that takes a parser and applies it only and only if the
-- indentation level is equal to the current indentation level
-- It fails if the indentation level is not the same
indentSame :: Int -> Parser a -> Parser a
indentSame ilevel p = try $ do
  level <- eol *> optional (try emptyLineWithEOL) *> consumeIndents
  if level == ilevel
    then local (const level) p
    else do
      e <- optional $ indentSame ilevel p
      case e of
        Just x -> return x
        Nothing ->
          fail $
            "Indentation level mismatch, expected "
              ++ show ilevel
              ++ " but received "
              ++ show level

sameLine :: Parser a -> Parser a
sameLine p = try $ do
  lineNumber <- sourceLine <$> getSourcePos
  x <- p
  lineNumberEnd <- sourceLine <$> getSourcePos
  if lineNumber == lineNumberEnd
    then return x
    else
      fail $
        "Expected to be on the same line as line "
          ++ show lineNumber
          ++ " but received line "
          ++ show lineNumberEnd

-- Indent parser that takes a parser and applies it only and only if there is
-- no indentation.
-- This indent sensitive parsing function is quite special as it does not
-- consume any newlines. Often used to parse top-level constructs.
nonIndented :: Parser a -> Parser (Maybe a)
nonIndented p = do
  ilevel <- consumeIndents
  if ilevel == 0
    then Just <$> local (const 0) p
    else do
      p' <- optional (notFollowedBy eol)
      case p' of
        Just _ -> return Nothing
        Nothing -> fail $ "Indentation level mismatch, expected 0 but received " ++ show ilevel
