module System.IO.Pretty (
  printText,
  ppPut,
  ppPrint,
  ppSuccess,
  ppFailure,
  ppBuilding,
) where

import Data.Text.IO
import Plume.Syntax.Internal.Pretty.ANSI
import Prettyprinter.Render.Terminal
import Prelude hiding (print, putStrLn, readFile)

printText :: (ToText a) => a -> IO ()
printText = putStrLn . toText

ppPut :: (ANSIPretty a) => a -> IO ()
ppPut = putDoc . ansiPretty

ppPrint :: (ANSIPretty a) => a -> IO ()
ppPrint x = ppPut x >> putStrLn ""

ppSuccess :: Text -> IO ()
ppSuccess x = putDoc (anColDull Green "✓" <+> pretty x) >> putStrLn ""

ppFailure :: Text -> IO ()
ppFailure x = putDoc (anColDull Red "✗" <+> pretty x) >> putStrLn ""

ppBuilding :: Text -> IO ()
ppBuilding x = putDoc (anColDull Yellow "⚙" <+> pretty x) >> putStrLn ""