module Plume.Syntax.Parser.Modules.Type where

import Control.Monad.Parser
import Plume.Syntax.Common.Type
import Plume.Syntax.Concrete
import Plume.Syntax.Parser.Lexer
import Text.Megaparsec

-- | Parse a primitive type
-- | A primitive type is a type that is built-in to the language
-- |
-- | example: int, bool, str, char, float
tPrimitive :: Parser PlumeType
tPrimitive =
  choice
    [ symbol "int" $> TInt
    , symbol "bool" $> TBool
    , symbol "str" $> TString
    , symbol "char" $> TChar
    , symbol "float" $> TFloat
    ]

-- | Parse a function type
-- | A function type is a type that represents a function
-- | 
-- | SYNTAX:
-- |  - fn (t1, t2, ..., tn): return
-- | where t1, t2, ..., tn are the argument types and return is the return type
tFunction :: Parser PlumeType
tFunction = do
  void $ reserved "fn"
  args <- parens (tType `sepBy` comma) <* symbol ":"
  TFunction args <$> tType

-- | Parse a tuple type
-- | A tuple type is a type that represents a tuple
-- | 
-- | SYNTAX:
-- |  - (t1, t2, ..., tn) <=> tuple of t1, t2, ..., tn
-- |  - () <=> unit type
-- |  - (t) <=> t
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

-- | Parse a list type
-- | A list type is a type that represents a list
-- |
-- | SYNTAX: [t] with t being the type of the list
tList :: Parser PlumeType
tList = TList <$> brackets tType

-- | Parse a type constructor
-- | A type constructor is a type that is defined by the user and that is
-- | parameterized by other types
-- | 
-- | SYNTAX: C<t1, t2, ..., tn> where C is the type 
-- |         constructor and t1, t2, ..., tn
tCon :: Parser PlumeType
tCon = do
  c <- identifier
  tys <- angles (tType `sepBy1` comma)
  return (TCon c tys)

-- | Parse a type identifier
-- | A type identifier is just an identifier that can either represent a type
-- | or a type variable
tId :: Parser PlumeType
tId = TId <$> identifier

-- | Parse a mutable type
-- | A mutable type is a type that represents a mutable reference to 
-- | another type
-- |
-- | SYNTAX: mut t where t is the type of the mutable reference
tMut :: Parser PlumeType
tMut = TMut <$> (reserved "mut" *> tType)

tVariableType :: Parser PlumeType
tVariableType = do
  void $ symbol ".."
  TCon "variable" . (:[]) <$> tType

-- | Parse a type
-- | A type is a type that can be used in the language
-- | A type can be a primitive type, a function type, a tuple type, a list type,
-- | a type constructor, a type identifier or a mutable type
tType :: Parser PlumeType
tType =
  choice
    [ tFunction
    , tTuple
    , tMut
    , tVariableType
    , tPrimitive
    , tList
    , -- Try may be used here because type application starts with an identifier
      -- just like type identifier
      try tCon
    , tId
    ]

-- | Parse a generic type
-- | A generic type is a type that is parameterized by a type variable
-- | Currently, only type variables are supported as generic types
-- | But in the future, this may be extended to support other type constraints
-- | such as type extension constraints
parseGeneric :: Parser PlumeGeneric
parseGeneric = do
  name <- identifier
  extension <- optional parseClss
  case extension of
    Just exts -> return (GExtends name exts)
    Nothing -> return (GVar name)

  where
    parseClss :: Parser [PlumeInterface]
    parseClss = do
      void $ reserved "extends"
      choice [
          pure <$> parseCls,
          parens (parseCls `sepBy1` comma)
        ]

    parseCls :: Parser PlumeInterface
    parseCls = do
      choice [
          parseLonelyClass,
          parseCls'
        ]

    parseLonelyClass :: Parser PlumeInterface
    parseLonelyClass = do
      name <- identifier
      return $ Interface name []
    
    parseCls' :: Parser PlumeInterface
    parseCls' = do
      name <- identifier
      exts <- angles (tType `sepBy1` comma)
      let exts' = Interface name exts
      return exts'

-- | Parse a type constructor
-- | A type constructor is a sort of function that takes types as arguments
-- | and returns the type that is constructed by the type constructor
typeConstructor :: Parser (TypeConstructor PlumeType)
typeConstructor =
  choice
    [ try $ TConstructor <$> identifier <*> parens (tType `sepBy` comma)
    , TVariable <$> identifier
    ]
