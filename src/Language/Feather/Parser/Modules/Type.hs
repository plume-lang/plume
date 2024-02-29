module Language.Feather.Parser.Modules.Type where

import Data.Functor
import Data.Maybe
import Language.Feather.CST.Type
import Language.Feather.Parser.Lexer
import Text.Megaparsec
import Text.Megaparsec.Char

-- Primitive types parsing function
-- Primitive types are just types that are not using other types
-- to build themselves. They're final types as they can't be destructured.
tPrimitive :: Parser ConcreteType
tPrimitive =
  choice
    [ symbol "int" $> TInt,
      symbol "bool" $> TBool,
      symbol "str" $> TString,
      symbol "char" $> TChar,
      symbol "float" $> TFloat
    ]

-- (t1, t2, ..., tn) -> ret where t1, t2, ..., tn are the function type arguments types
-- and ret is the function type return type
tFunction :: Parser ConcreteType
tFunction = do
  args <- parens (tType `sepBy` comma)
  _ <- symbol "->"
  ret <- tType
  return (TFunction args ret)

-- (t1, t2, ..., tn) where t1, t2, ..., tn are the tuple type elements. There are tuple
-- special cases depending on the quantity of types specified for the tuple:
-- - Tuples with 0 argument are just unit, void types (as Haskell does)
-- - Tuples with 1 argument are just parenthesized types
-- - Tuples with n elements are real tuples
tTuple :: Parser ConcreteType
tTuple = do
  tys <- parens (tType `sepBy` comma)
  return $
    case tys of
      [] -> TUnit
      [x] -> x
      _ -> TTuple tys

-- [t] where t is a concrete type. It represents list where the list's elements
-- are of t's type
tList :: Parser ConcreteType
tList =
  TList <$> brackets tType

-- 'a where a is an identifier is used in order to build type variables (the most basic
-- type component used to deal with generic programming)
tVar :: Parser ConcreteType
tVar = char '\'' *> (TVar <$> identifier)

-- x<t1, t2, ..., tn> where x is an identifier (resp. datatype name) and t1, t2, ... tn
-- are the datatype arguments. This is used to represent type applications over user-defined
-- datatypes (such as ADTs, or even GADTs...)
tCon :: Parser ConcreteType
tCon = do
  c <- identifier
  tys <- angles (tType `sepBy1` comma)
  return (TCon c tys)

tId :: Parser ConcreteType
tId = TId <$> identifier

tRecord :: Parser ConcreteType
tRecord = braces $ do
  fields <-
    sepBy1
      ( do
          l <- identifier
          _ <- colon
          t <- tType
          return (l, t)
      )
      comma
  r <- optional $ symbol "|" *> tType
  let rec = foldl (\acc (l, t) -> TRowExtend l t acc) (fromMaybe TRowEmpty r) fields
  return $ TRecord rec

tRowEmpty :: Parser ConcreteType
tRowEmpty = symbol "..." $> TRowEmpty

-- Main type parsing function
tType :: Parser ConcreteType
tType =
  choice
    [ -- Try may be used here because function type starts with the same
      -- syntax as tuple
      tRecord,
      tRowEmpty,
      try tFunction,
      tPrimitive,
      tList,
      tTuple,
      tVar,
      try tCon,
      tId
    ]
