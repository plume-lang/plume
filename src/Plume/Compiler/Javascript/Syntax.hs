{-# LANGUAGE PatternSynonyms #-}
module Plume.Compiler.Javascript.Syntax where

import Plume.Syntax.Common.Literal
import GHC.Show
import Prelude hiding (show)
import Data.Char
import Text.Printf

type Name = Text
type Field = Text

data Statement
  = JSIfStatement Expression [Statement] (Maybe [Statement])
  | JSVariableDeclaration Name Expression
  | JSExpression Expression
  | JSFunction Name [Name] [Statement]
  | JSReturn Expression
  | JSUpdate Update Expression
  | JSAsyncFunction Name [Name] [Statement]

data Update
  = JSFieldUpdate Field Update
  | JSVariable Name

type Operator = Text

data Expression
  = JSLiteral Literal
  | JSIdentifier Name
  | JSBinary Operator Expression Expression
  | JSUnary Operator Expression
  | JSCall Expression [Expression]
  | JSObject [(Field, Expression)]
  | JSArray [Expression]
  | JSArrayIndex Expression Expression
  | JSTernary Expression Expression Expression
  | JSNull
  | JSAnnFunction [Name] [Statement]
  | JSAwait Expression
  | JSAsyncAnnFunction [Name] [Statement]

pattern JSMember :: Expression -> Field -> Expression
pattern JSMember e f = JSArrayIndex e (JSLiteral (LString f))

pattern JSSlice :: Expression -> Integer -> Expression
pattern JSSlice e i = JSCall (JSMember e "slice") [JSLiteral (LInt i)]

newtype Program = Program [Statement]
  deriving (Monoid, Semigroup)

instance Show Program where
  show (Program stmts) = intercalate "\n" (fmap show stmts)

instance Show Statement where
  show (JSIfStatement e s1 s2) = "if (" <> show e <> ") {\n" <> intercalate "\n" (fmap show s1) <> "\n} else {\n" <> intercalate "\n" (fmap show (fromMaybe [] s2)) <> "\n}"
  show (JSVariableDeclaration n e) = "var " <> varify (toString n) <> " = " <> show e <> ";"
  show (JSExpression e) = show e <> ";"
  show (JSFunction n args body) = "function " <> varify (toString n) <> "(" <> intercalate ", " (fmap (varify . toString) args) <> ") {\n" <> intercalate "\n" (fmap show body) <> "\n}"
  show (JSReturn e) = "return " <> show e <> ";"
  show (JSUpdate u e) = show u <> " = " <> show e <> ";"
  show (JSAsyncFunction n args body) = "async function " <> varify (toString n) <> "(" <> intercalate ", " (fmap (varify . toString) args) <> ") {\n" <> intercalate "\n" (fmap show body) <> "\n}"

instance Show Update where
  show (JSFieldUpdate f u) = show u <> "." <> toString f
  show (JSVariable n) = varify $ toString n

doesContainReturn :: [Statement] -> Bool
doesContainReturn = any isReturn
  where
    isReturn (JSReturn _) = True
    isReturn _ = False

isIdent :: Char -> Bool
isIdent x = isLetter x || x == '_' || x == '$'

varify :: String -> String
varify "require" = "require"
varify "process" = "process"
varify x = "$" ++ concatMap (\x' -> if isIdent x' then [x'] else show (ord x')) x

encodeUnicode16 :: String -> String
encodeUnicode16 = concatMap escapeChar
  where
    escapeChar c
      | c == '\"' = "\\\""
      | c == '\'' = "\\\'"
      | ' ' <= c && c <= 'z' = [c]
      | isPrint c = [c]
      | otherwise = printf "\\u%04x" (fromEnum c)

instance Show Expression where
  show (JSLiteral l) = case l of
    LInt i -> show i
    LFloat f -> show f
    LString s -> "\"" <> encodeUnicode16 (toString s) <> "\""
    LBool b -> if b then "true" else "false"
    LChar c -> "'" <> encodeUnicode16 [c] <> "'"
  show (JSIdentifier n) = varify $ toString n
  show (JSBinary o e1 e2) = "(" <> show e1 <> " " <> toString o <> " " <> show e2 <> ")"
  show (JSUnary o e) = "(" <> toString o <> show e <> ")"
  show (JSCall e args) = show e <> "(" <> intercalate ", " (fmap show args) <> ")"
  show (JSObject fields) = "({ " <> intercalate ", " (fmap (\(f, e) -> show f <> ": " <> show e) fields) <> " })"
  show (JSArray es) = "[ " <> intercalate ", " (fmap show es) <> " ]"
  show (JSArrayIndex e i) = show e <> "[" <> show i <> "]"
  show (JSTernary e1 e2 e3) = "(" <> show e1 <> " ? " <> show e2 <> " : " <> show e3 <> ")"
  show JSNull = "null"
  show (JSSlice e i) = show e <> "[" <> show i <> ":]"
  show (JSAnnFunction args body) = "(function(" <> intercalate ", " (fmap show args) <> ") {\n" <> intercalate "\n" (fmap show body) <> "\n})"
  show (JSAwait e) = "(await " <> show e <> ")"
  show (JSAsyncAnnFunction args body) = "(async function(" <> intercalate ", " (fmap show args) <> ") {\n" <> intercalate "\n" (fmap show body) <> "\n})"
