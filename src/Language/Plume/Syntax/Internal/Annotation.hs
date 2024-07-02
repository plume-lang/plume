module Language.Plume.Syntax.Internal.Annotation where

import GHC.Show qualified as S

data Annotation t = MkAnnotation {name :: Text, value :: t}
  deriving (Eq, Ord)

instance (Show t) => Show (Annotation t) where
  show (MkAnnotation n v) = toString n <> ": " <> S.show v

instance (ToText t) => ToText (Annotation t) where
  toText (MkAnnotation n v) = n <> ": " <> toText v

instance Foldable Annotation where
  foldMap f (MkAnnotation _ v) = f v

instance Functor Annotation where
  fmap f (MkAnnotation n v) = MkAnnotation n (f v)

instance Traversable Annotation where
  traverse f (MkAnnotation n v) = MkAnnotation n <$> f v
