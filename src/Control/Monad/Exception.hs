{-# LANGUAGE DefaultSignatures #-}

module Control.Monad.Exception where

import Control.Monad.Parser
import Plume.Syntax.Internal.Pretty.ANSI
import Prettyprinter.Render.Terminal.Internal
import System.IO.Pretty
import Text.Megaparsec

class Throwable a where
  showError :: a -> Text
  default showError :: (ANSIPretty a) => a -> Text
  showError e =
    renderStrict $
      layoutPretty defaultLayoutOptions $
        ansiPretty e

instance Throwable Text where
  showError err =
    toText $
      anBold (anCol Red "[error]") <+> pretty err

instance Throwable ParsingError where
  showError = toText . errorBundlePretty

-- catchIO is an utility function that aims at catching function that may
-- throw an error during execution and automatically print the error and
-- exiting the program with a failure status code.
-- If no error is thrown, the function will continue its execution by
-- applying the provided function to the right result.
with
  :: (MonadIO m, Throwable err)
  => m (Either err a)
  -> (a -> m b)
  -> m b
with m f = do
  x <- m
  case x of
    Left e -> liftIO $ do
      printText (showError e)
      exitFailure
    Right r -> f r

infix 1 `with`
