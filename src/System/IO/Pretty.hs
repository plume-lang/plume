module System.IO.Pretty (
  printText,
  ppPut,
  ppPrint,
  ppSuccess,
  ppFailure,
  ppBuilding,
  parseError,
  printErrorFromString,
) where

import Data.Text.IO
import Plume.Syntax.Internal.Pretty.ANSI
import Prettyprinter.Render.Terminal
import Prelude hiding (print, putStrLn, readFile)
import Error.Diagnose qualified as D
import Error.Diagnose.Compat.Megaparsec qualified as D
import Control.Monad.Parser (FileContent, ParsingError)
import Plume.Syntax.Concrete.Expression
import System.Directory (doesFileExist)
import qualified Data.Maybe as Mb
import Text.Megaparsec.Pos (unPos, SourcePos (..))

instance D.HasHints Void String where
  hints _ = mempty

printText :: (ToText a) => a -> IO ()
printText = putStrLn . toText

ppPut :: (ANSIPretty a) => a -> IO ()
ppPut = putDoc . ansiPretty

ppPrint :: (ANSIPretty a) => a -> IO ()
ppPrint x = ppPut x >> putStrLn ""

ppSuccess :: Text -> IO ()
ppSuccess x = putDoc (anBold (anCol Green "[success]") <> ":" <+> pretty x) >> putStrLn ""

ppFailure :: Text -> IO ()
ppFailure x = putDoc (anBold (anCol Red "[error]") <> ":" <+> pretty x) >> putStrLn ""

ppBuilding :: Text -> IO ()
ppBuilding x = putDoc (anBold (anCol Yellow "[build]") <> ":" <+> pretty x) >> putStrLn ""

parseError :: ParsingError -> FilePath -> FileContent -> IO ()
parseError err' fp fc = do
  let diag :: D.Diagnostic String = D.errorDiagnosticFromBundle Nothing "Parse error on input" Nothing err'
      diag' = D.addFile diag fp (toString fc)
    in D.printDiagnostic stderr True True 4 D.defaultStyle diag'

printErrorFromString :: Maybe Text -> (String, Maybe String, Position) -> String -> IO ()
printErrorFromString content (error', msg, (p1, p2)) step = do
  let p1' = (unPos p1.sourceLine, unPos p1.sourceColumn)
  let p2' = (unPos p2.sourceLine, unPos p2.sourceColumn)
  let file' = p1.sourceName
  b <- doesFileExist file'
  x' <- toString <$> if b then readFile file' else return $ Mb.fromJust content
  let pos' = D.Position p1' p2' p1.sourceName
  let beautifulExample = D.err
        Nothing
        error'
        [ (pos', D.This step) ]
        (maybeToList msg)

  -- Create the diagnostic 
  let diagnostic  = D.addFile D.def file' x'
  let diagnostic' = D.addReport diagnostic beautifulExample

  -- Print with unicode characters, colors and the default style
  D.printDiagnostic stdout True True 4 D.defaultStyle diagnostic'