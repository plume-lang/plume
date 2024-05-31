{-# LANGUAGE PatternSynonyms #-}

module Plume.Syntax.Common.Annotation where

import GHC.Records
import GHC.Show
import Prelude hiding (show)

type IsMutable = Bool
type IsMacro = Bool

-- | An annotation is a piece of metadata that is attached to a value.
-- | It is used to provide additional information about the value.
-- | It is for instance used in the AST and CST to provide optional
-- | support for type information.
data Identifier = MkIdentifier {identifier :: Text, isMacro :: IsMacro}
  deriving (Eq)

data Annotation t = Annotation {annotationName :: Identifier, annotationValue :: t, isMutable :: IsMutable}
  deriving (Eq, Functor)

instance IsString Identifier where
  fromString x = MkIdentifier (fromString x) False

fromText :: Text -> Identifier
fromText x = MkIdentifier x False

instance Show Identifier where
  show (MkIdentifier x isMacro)
    | isMacro = "@" <> toString x
    | otherwise = toString x

instance Show t => Show (Annotation t) where
  show (Annotation n x mut)
    | mut = "mut " <> show n <> " : " <> show x
    | otherwise = show n <> " : " <> show x

-- | Derive the HasField instance for the Annotation type
-- | Enabling us to use the `annotationName` and `annotationValue` functions
-- | as dot operators.
deriveHasField ''Annotation
deriveHasField ''Identifier

-- | Pattern synonym for the Annotation type
-- | x :@: y is equivalent to Annotation x y
pattern (:@:) :: Identifier -> t -> Annotation t
pattern name :@: value = Annotation name value False

instance Applicative Annotation where
  pure x = Annotation "" x False
  Annotation _ f b1 <*> Annotation n x b2 = Annotation n (f x) (b1 || b2)

instance Monad Annotation where
  Annotation _ x _ >>= f = f x

instance Traversable Annotation where
  traverse f (Annotation n x b) = Annotation n <$> f x <*> pure b

instance Foldable Annotation where
  foldMap f (Annotation _ x _) = f x