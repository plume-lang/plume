module Language.Feather.Parser.Modules.Operator where

import Control.Applicative
import Control.Monad.Combinators.Expr
import Language.Feather.CST
import Language.Feather.Parser.Lexer

-- Useful shortcut functions for defining operators

binary :: String -> (Expression -> Expression -> Expression) -> Operator Parser Expression
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: String -> (Expression -> Expression) -> Operator Parser Expression
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

-- makeUnaryOp is a function that mainly takes a parser that returns a function
-- (such as postfix function previously defined) and returns the same function
-- but with the chaining capacity
makeUnaryOp :: (Alternative f) => f (a -> a) -> f (a -> a)
makeUnaryOp s = foldr1 (.) . reverse <$> some s

-- Actual feather operators definition
-- Reverse is used in order to define operators from low to high precedence.
operators :: [[Operator Parser Expression]]
operators =
  reverse
    [ [ binary "and" (EBinary And),
        binary "or" (EBinary Or)
      ],
      [ binary "==" (EBinary Equals),
        binary "!=" (EBinary NotEquals),
        binary ">=" (EBinary GreaterThan),
        binary "<=" (EBinary LesserThan),
        binary ">" (EBinary StrictlyGreatherThan),
        binary "<" (EBinary StrictlyLesserThan)
      ],
      [ binary "+" (EBinary Plus),
        binary "-" (EBinary Minus)
      ],
      [ binary "*" (EBinary Times),
        binary "/" (EBinary Division)
      ],
      [binary "%" (EBinary Mod)],
      [prefix "not" (EPrefix Not)],
      [ Postfix $ makeUnaryOp $ do
          _ <- symbol "\\"
          name <- identifier
          return $ \e -> ERowRestrict e name
      ],
      [ Postfix $ makeUnaryOp $ do
          _ <- symbol "."
          name <- identifier
          return $ \e -> ERowSelect e name
      ]
    ]
