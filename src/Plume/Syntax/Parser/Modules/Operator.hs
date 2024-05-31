{-# LANGUAGE PatternSynonyms #-}

module Plume.Syntax.Parser.Modules.Operator where

import Control.Monad.Combinators.Expr
import Data.SortedList qualified as SL
import Control.Monad.Parser
import Data.Foldable hiding (elem)
import Plume.Syntax.Concrete
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Parser.Lexer
import Control.Monad.Exception (compilerError)

-- OPERATOR SHORTHANDS

binary
  :: Text -> (Expression -> Expression -> Expression) -> Operator Parser Expression
binary name f = InfixL (f <$ symbol name)

prefix
  , postfix
    :: Text -> (Expression -> Expression) -> Operator Parser Expression
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

-- | Make a unary operator sequence
-- | A unary operator sequence is a sequence of unary operators
-- | that are applied to an expression.
-- |
-- | example: not not a <=> not (not a)
-- |
-- | The sequence is applied from right to left.
-- | This is because the postfix and prefix operators are applied *
-- | from right to left.
makeUnaryOp :: (Alternative f) => f (a -> a) -> f (a -> a)
makeUnaryOp s = foldr1 (.) . reverse <$> some s

-- Actual Plume operators definition
-- Reverse is used in order to define operators from low to high precedence.
operators :: [[Operator Parser Expression]]
operators =
  reverse
    [
      [ binary "and" (EBinary "and")
      , binary "or" (EBinary "or")
      ]
    , [prefix "not" (EPrefix "not")]
    ,
      [ binary "==" (EBinary "==")
      , binary "!=" (EBinary "!=")
      , binary ">=" (EBinary ">=")
      , binary "<=" (EBinary "<=")
      , binary ">" (EBinary ">")
      , binary "<" (EBinary "<")
      ]
    ,
      [ binary "+" (EBinary "+")
      , binary "-" (EBinary "-")
      ]
    ,
      [ binary "*" (EBinary "*")
      , binary "/" (EBinary "/")
      ]
    , [binary "%" (EBinary "%")]
    , [prefix "*" EUnMut]
    ]

-- | Sort custom operators
-- | Custom operators are sorted and then parsed into a list of operator
-- | that can be used by the expression parser.
sortCustomOperators :: SL.SortedList CustomOperator -> [[Operator Parser Expression]]
sortCustomOperators ops = map ((: []) . parseOperator) (SL.fromSortedList ops)
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
        compilerError "Invalid operator type"

pattern MkVar :: Text -> Expression
pattern MkVar name = EVariable (MkIdentifier name False) Nothing

pattern CustomInfix :: Text -> Expression -> Expression -> Expression
pattern CustomInfix name e1 e2 = EApplication (MkVar name) [e1, e2]

pattern CustomPrefix, CustomPostfix :: Text -> Expression -> Expression
pattern CustomPrefix name e = EApplication (MkVar name) [e]
pattern CustomPostfix name e = EApplication (MkVar name) [e]
