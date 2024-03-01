{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Feather.CST.Annotation where

-- Annotations are used to store additional information about variables.
-- It's a simple key-value pair, where the key is a string and the value is a generic type.
-- That generic type can be anything from a type to a literal to another annotation.

data Annotation t = Annotation
  { annotationName :: Text,
    annotationValue :: t
  }
  deriving (Eq, Ord, Functor)

pattern (:@:) :: Text -> t -> Annotation t
pattern name :@: value = Annotation name value

instance {-# OVERLAPS #-} (ToText t) => ToText (Annotation (Maybe t)) where
  toText (Annotation name value) = case value of
    Nothing -> name
    Just t -> name <> ": " <> toText t

instance (ToText t) => ToText (Annotation t) where
  toText (Annotation name value) = name <> ": " <> toText value
