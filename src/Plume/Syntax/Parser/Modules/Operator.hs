module Plume.Syntax.Parser.Modules.Operator where

import Control.Monad.Combinators.Expr
import Control.Monad.Parser
import Data.Foldable
import Plume.Syntax.Concrete
import Plume.Syntax.Parser.Lexer

-- Useful shortcut functions for defining operators

binary :: Text -> (Expression -> Expression -> Expression) -> Operator Parser Expression
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expression -> Expression) -> Operator Parser Expression
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

-- makeUnaryOp is a function that mainly takes a parser that returns a function
-- (such as postfix function previously defined) and returns the same function
-- but with the chaining capacity
makeUnaryOp :: (Alternative f) => f (a -> a) -> f (a -> a)
makeUnaryOp s = foldr1 (.) . reverse <$> some s

-- Actual Plume operators definition
-- Reverse is used in order to define operators from low to high precedence.
operators :: [[Operator Parser Expression]]
operators =
  reverse
    [
      [ binary "and" (EBinary And)
      , binary "or" (EBinary Or)
      ]
    , [prefix "not" (EPrefix Not)]
    ,
      [ binary "==" (EBinary Equals)
      , binary "!=" (EBinary NotEquals)
      , binary ">=" (EBinary GreaterThan)
      , binary "<=" (EBinary LesserThan)
      , binary ">" (EBinary StrictlyGreatherThan)
      , binary "<" (EBinary StrictlyLesserThan)
      ]
    ,
      [ binary "+" (EBinary Plus)
      , binary "-" (EBinary Minus)
      ]
    ,
      [ binary "*" (EBinary Times)
      , binary "/" (EBinary Division)
      ]
    , [binary "%" (EBinary Mod)]
    ]
