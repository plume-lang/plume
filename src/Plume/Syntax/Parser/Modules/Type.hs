module Plume.Syntax.Parser.Modules.Type where

import Control.Monad.Parser
import Plume.Syntax.Common.Type
import Plume.Syntax.Concrete
import Plume.Syntax.Parser.Lexer
import Text.Megaparsec

-- Primitive types parsing function
-- Primitive types are just types that are not using other types
-- to build themselves. They're final types as they can't be destructured.
tPrimitive :: Parser PlumeType
tPrimitive =
  choice
    [ symbol "int" $> TInt
    , symbol "bool" $> TBool
    , symbol "str" $> TString
    , symbol "char" $> TChar
    , symbol "float" $> TFloat
    ]

-- (t1, t2, ..., tn) -> ret where t1, t2, ..., tn are the function type arguments types
-- and ret is the function type return type
tFunction :: Parser PlumeType
tFunction = do
  void $ reserved "fn"
  args <- parens (tType `sepBy` comma) <* symbol ":"
  TFunction args <$> tType

-- (t1, t2, ..., tn) where t1, t2, ..., tn are the tuple type elements. There are tuple
-- special cases depending on the quantity of types specified for the tuple:
-- - Tuples with 0 argument are just unit, void types (as Haskell does)
-- - Tuples with 1 argument are just parenthesized types
-- - Tuples with n elements are real tuples
tTuple :: Parser PlumeType
tTuple = do
  tys <- parens (tType `sepBy` comma)
  return $
    case tys of
      [] -> TUnit
      [x] -> x
      _ -> buildTuple tys
 where
  buildTuple [] = TUnit
  buildTuple [x] = x
  buildTuple (x : xs) = TTuple [x, buildTuple xs]

-- [t] where t is a concrete type. It represents list where the list's elements
-- are of t's type
tList :: Parser PlumeType
tList =
  TList <$> brackets tType

-- x<t1, t2, ..., tn> where x is an identifier (resp. datatype name) and t1, t2, ... tn
-- are the datatype arguments. This is used to represent type applications over user-defined
-- datatypes (such as ADTs, or even GADTs...)
tCon :: Parser PlumeType
tCon = do
  c <- identifier
  tys <- angles (tType `sepBy1` comma)
  return (TCon c tys)

tId :: Parser PlumeType
tId = TId <$> identifier

tMut :: Parser PlumeType
tMut = TMut <$> (reserved "mut" *> tType)

-- Main type parsing function
tType :: Parser PlumeType
tType =
  choice
    [ tFunction
    , tTuple
    , tMut
    , tPrimitive
    , tList
    , -- Try may be used here because type application starts with an identifier
      -- just like type identifier
      try tCon
    , tId
    ]

parseGeneric :: Parser PlumeGeneric
parseGeneric = GVar <$> identifier

typeConstructor :: Parser (TypeConstructor PlumeType)
typeConstructor =
  choice
    [ try $ TConstructor <$> identifier <*> parens (tType `sepBy` comma)
    , TVariable <$> identifier
    ]
