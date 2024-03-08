module Plume.Syntax.Translation.Generics where

-- A translator is a utility function type that takes a main converter
-- function and a value to convert. It returns an IO action that
-- converts the value to another type.
type Translator a b = (a -> IO b) -> a -> IO b
