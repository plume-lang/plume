{-# LANGUAGE PatternSynonyms #-}

module Language.Feather.CST.Annotation where

-- Annotations are used to store additional information about variables.
-- It's a simple key-value pair, where the key is a string and the value is a generic type.
-- That generic type can be anything from a type to a literal to another annotation.

data Annotation t = Annotation
  { annotationName :: String,
    annotationValue :: t
  }
  deriving (Eq, Ord, Functor)

pattern (:@:) :: String -> t -> Annotation t
pattern name :@: value = Annotation name value

instance {-# OVERLAPS #-} (Show t) => Show (Annotation (Maybe t)) where
  show (Annotation name value) = case value of
    Nothing -> name
    Just t -> name ++ ": " ++ show t

instance (Show t) => Show (Annotation t) where
  show (Annotation name value) = name ++ ": " ++ show value
