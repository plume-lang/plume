{-# LANGUAGE LambdaCase #-}

module Plume.Syntax.Translation.Generics where

import Control.Monad.Exception
import Control.Monad.IO
import Data.SortedList qualified as SL
import Control.Monad.Parser
import GHC.IO
import Plume.Syntax.Concrete.Expression (Position)
import Plume.Syntax.Parser.Lexer

{-# NOINLINE stdPath #-}
stdPath :: IORef (Maybe FilePath)
stdPath = unsafePerformIO $ newIORef Nothing

{-# NOINLINE positionRef #-}
positionRef :: IORef (Maybe Position)
positionRef = unsafePerformIO $ newIORef Nothing

{-# NOINLINE operators #-}
operators :: IORef (SL.SortedList CustomOperator)
operators = unsafePerformIO $ newIORef mempty

-- A spreadable type is a type that can handle either a single value or a
-- spread of values. It is used to specify the translator that we need to
-- spread the requested module in the main module.
data Spreadable a b
  = Spread a
  | Single b
  | Empty

data Error
  = CompilerError Text -- Used to add some context
  | MacroNotFound Text Position
  | -- [Text] are the macro arguments, Int is the number of actual arguments
    ArgumentsMismatch [Text] Int Position
  | ModuleNotFound Text Position
  | NoPositionSaved -- Used when the parser does not save the position
  | ParserError ParsingError

instance Throwable Error where
  showError = \case
    CompilerError err -> err
    MacroNotFound name pos ->
      "Macro " <> name <> " not found at " <> show pos
    ArgumentsMismatch args n pos ->
      "Invalid number of arguments for macro at "
        <> show pos
        <> ". Expected "
        <> show (length args)
        <> " but got "
        <> show n
    ModuleNotFound name pos ->
      "Module " <> name <> " not found at " <> show pos
    NoPositionSaved -> "No position saved"
    ParserError err -> showError err

-- A translator error is a type that represents an error that can occur
-- during the translation process. It is a type alias for an Either type
-- that takes an error type and a value type.
type TranslatorError err b = Either err (Spreadable [b] b)

type IsImportConversion = Bool

-- A translator reader is a type that represents a reader monad that takes
-- a file path and a translator error type.
type TranslatorReader err b =
  IOReader (FilePath, IsImportConversion) (TranslatorError err b)

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

throwError' :: err -> IOReader (FilePath, IsImportConversion) (Either err b)
throwError' = return . Left

{-# NOINLINE imports #-}
imports :: IORef [FilePath]
imports = unsafePerformIO $ newIORef []

-- ShouldBeAlone checks if a value is a single value or a spread of values.
-- If it is a spread of values that hold only one value, it returns the
-- value. If it is a single value, it returns the value. Otherwise, it
-- returns an error.
shouldBeAlone :: Either Error (Spreadable [a] a) -> Either Error a
shouldBeAlone (Right (Single x)) = Right x
shouldBeAlone (Right (Spread [x])) = Right x
shouldBeAlone (Left err) = Left err
shouldBeAlone _ = Left (CompilerError "Expected a single value")

-- Flat let us flatten a list of spreadable values into a single list of
-- values.
flat :: [Spreadable [a] a] -> [a]
flat = concatMap $ \case
  Spread a -> a
  Single a -> [a]
  Empty -> []

mapSpreadable :: (a -> b) -> Spreadable [a] a -> Spreadable [b] b
mapSpreadable f = \case
  Spread a -> Spread $ map f a
  Single a -> Single $ f a
  Empty -> Empty

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

withMaybePos
  :: (Throwable err)
  => Maybe Position
  -> TranslatorReader err a
  -> TranslatorReader err a
withMaybePos (Just pos) f = withPosition pos f
withMaybePos Nothing f = f

withPosition
  :: (Throwable err) => Position -> TranslatorReader err a -> TranslatorReader err a
withPosition pos f = do
  old <- readIORef positionRef
  writeIORef positionRef (Just pos)

  res <-
    f `with` \case
      Single e' -> bireturn (Single e')
      Spread es -> bireturn (Spread es)
      Empty -> bireturn Empty

  writeIORef positionRef old
  return res