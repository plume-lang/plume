{-# LANGUAGE LambdaCase #-}

module Plume.Syntax.Translation.Generics where

import Control.Monad.Exception
import Control.Monad.IO

-- A spreadable type is a type that can handle either a single value or a
-- spread of values. It is used to specify the translator that we need to
-- spread the requested module in the main module.
data Spreadable a b
  = Spread a
  | Single b
  | Empty

-- A translator error is a type that represents an error that can occur
-- during the translation process. It is a type alias for an Either type
-- that takes an error type and a value type.
type TranslatorError err b = Either err (Spreadable [b] b)

-- A translator reader is a type that represents a reader monad that takes
-- a file path and a translator error type.
type TranslatorReader err b = IOReader FilePath (TranslatorError err b)

-- A translator is a utility function type that takes a main converter
-- function and a value to convert. It returns an IO action that
-- converts the value to another type.
type Translator err a b =
  (a -> TranslatorReader err b)
  -> a
  -> TranslatorReader err b

-- Bireturn is a utility function that takes a value and returns a nested
-- monad of the value.
bireturn :: (Monad m1, Monad m2) => a -> m1 (m2 a)
bireturn = return . return

-- TransRet is an utility function that is fully named as "transform
-- return". It is used as a return function for translators.
transRet
  :: (Monad m, Functor f) => f a -> m (f (Spreadable [a] a))
transRet = pure . fmap Single

-- MaybeM is a utility function that takes a function and a maybe value.
-- It is a monadic version of the maybe function.
maybeM :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f = maybe (return Nothing) (fmap Just . f)

throwError :: err -> TranslatorReader err b
throwError err = return $ Left err

-- ShouldBeAlone checks if a value is a single value or a spread of values.
-- If it is a spread of values that hold only one value, it returns the
-- value. If it is a single value, it returns the value. Otherwise, it
-- returns an error.
shouldBeAlone :: Either Text (Spreadable [a] a) -> Either Text a
shouldBeAlone (Right (Single x)) = Right x
shouldBeAlone (Right (Spread [x])) = Right x
shouldBeAlone (Left err) = Left err
shouldBeAlone _ = Left "Expected a single element"

-- Flat let us flatten a list of spreadable values into a single list of
-- values.
flat :: [Spreadable [a] a] -> [a]
flat = concatMap $ \case
  Spread a -> a
  Single a -> [a]
  Empty -> []

-- Monadic version of the concatMap function.
concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

-- FromSpreadable is a utility function that takes a spreadable value and
-- returns a list of values corresponding to the spreadable value.
fromSpreadable :: Spreadable [a] a -> [a]
fromSpreadable (Spread a) = a
fromSpreadable (Single a) = [a]
fromSpreadable Empty = []

-- Some logging instances for the spreadable type.
instance (ToText a, ToText b) => ToText (Spreadable a b) where
  toText (Spread a) = toText a
  toText (Single b) = toText b
  toText Empty = "Empty"

instance (Throwable err) => Throwable (Spreadable [err] err) where
  showError (Spread errs) = unlines $ map showError errs
  showError (Single err) = showError err
  showError Empty = "Empty"
