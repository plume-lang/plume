{-# LANGUAGE PatternSynonyms #-}

module Plume.Syntax.Common.Annotation where

import GHC.Records

-- | An annotation is a piece of metadata that is attached to a value.
-- | It is used to provide additional information about the value.
-- | It is for instance used in the AST and CST to provide optional
-- | support for type information.
data Annotation t = Annotation
  { annotationName :: Text
  , annotationValue :: t
  }
  deriving (Eq, Ord, Functor, Show)

-- | Derive the HasField instance for the Annotation type
-- | Enabling us to use the `annotationName` and `annotationValue` functions
-- | as dot operators.
deriveHasField ''Annotation

-- | Pattern synonym for the Annotation type
-- | x :@: y is equivalent to Annotation x y
pattern (:@:) :: Text -> t -> Annotation t
pattern name :@: value = Annotation name value

instance Applicative Annotation where
  pure = Annotation ""
  Annotation _ f <*> Annotation n x = Annotation n (f x)

instance Monad Annotation where
  Annotation _ x >>= f = f x

instance Traversable Annotation where
  traverse f (Annotation n x) = Annotation n <$> f x

instance Foldable Annotation where
  foldMap f (Annotation _ x) = f x