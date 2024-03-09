module Plume.Syntax.Translation.Generics where

-- A translator error is a type that represents an error that can occur
-- during the translation process. It is a type alias for an Either type
-- that takes an error type and a value type.
type TranslatorError err b = Either err b

-- A translator is a utility function type that takes a main converter
-- function and a value to convert. It returns an IO action that
-- converts the value to another type.
type Translator err a b =
  (a -> IO (TranslatorError err b))
  -> a
  -> IO (TranslatorError err b)
