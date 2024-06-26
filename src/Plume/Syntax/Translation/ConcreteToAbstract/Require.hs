{-# LANGUAGE LambdaCase #-}

module Plume.Syntax.Translation.ConcreteToAbstract.Require where

import Control.Monad.Exception
import Control.Monad.Parser
import Data.Set qualified as S
import Data.SortedList qualified as SL
import GHC.IO hiding (liftIO)
import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Concrete qualified as CST
import Plume.Syntax.Parser
import Plume.Syntax.Parser.Modules.ParseImports
import Plume.Syntax.Translation.Generics
import System.Directory
import System.FilePath
import System.Path.NameManip (guess_dotdot, absolute_path)
import Data.Maybe (fromJust)

filepathDifference :: FilePath -> FilePath -> FilePath
filepathDifference (x:xs) (y:ys) | x == y = filepathDifference xs ys
filepathDifference _ ys = ys

relativize :: FilePath -> FilePath -> FilePath
relativize from to = joinPath $ go (splitPath from) (splitPath to)
  where
    go [] ys = ys
    go xs [] = map (const "..") xs
    go (x:xs) (y:ys)
      | x == y    = go xs ys
      | otherwise = replicate (length (x:xs)) ".." ++ (y:ys)

-- | Absolutize a relative path to make it avaiable globally
absolutize :: String -> IO String
absolutize aPath
    | "~" `isPrefixOf` aPath = case aPath of
      (_:tail') -> do
        homePath <- getHomeDirectory
        return $ normalise $ addTrailingPathSeparator homePath 
                             ++ tail'
      _ -> return aPath
    | otherwise = do
        pathMaybeWithDots <- absolute_path aPath
        return $ fromJust $ guess_dotdot pathMaybeWithDots

-- | Store already parsed paths to avoid parsing them again
{-# NOINLINE parsedPaths #-}
parsedPaths :: IORef (Set FilePath)
parsedPaths = unsafePerformIO $ newIORef mempty

-- | Simple function to extract the value from an Either
-- | and return a default value if the Either is a Left
fromEither :: a -> Either b a -> a
fromEither _ (Right a) = a
fromEither a _ = a

-- | Get the path of a module according to its value:
-- |  - Can be a standard library module if prefixed with "std:"
-- |  - Can be a local module if not prefixed with "std:"
-- |  - Can be a module with a specific extension if the extension is provided,
-- |    for instance native functions are loaded from shared libraries with the
-- |    extension ".so" on Linux, ".dylib" on macOS and ".dll" on Windows.
getPath :: FilePath -> Maybe FilePath -> IOReader (FilePath, Bool) (FilePath, Maybe Text)
getPath modName ext = do
  cwd <- asks fst
  let strModName = toString modName -<.> fromMaybe "plm" ext
  let isStd = "std:" `isPrefixOf` modName
  let isMod = "mod:" `isPrefixOf` modName
  
  scope <- readIORef mode
  let newScope = case scope of
        _ | isStd -> Just "standard"
        _ | isMod -> Just "module"
        _ -> scope

  let modPath 
        | isStd = do
            p <- liftIO $ readIORef stdPath
            case p of
              Just p' -> return $ Right (p' </> drop 4 strModName)
              Nothing -> throwError' $ CompilerError "Standard library path not set"
        | isMod = do
            p <- liftIO $ readIORef modulePath
            case p of
              Just p' -> return $ Right (p' </> "modules" </> drop 4 strModName)
              Nothing -> throwError' $ CompilerError "PPM_PATH not set"
        | otherwise = return $ Right $ cwd </> strModName 


  res <- modPath `with` liftIO . absolutize

  let resWithoutExt = dropExtension res
  isDir <- liftIO (doesFileExist resWithoutExt) >>= \case
    False | isMod -> liftIO (doesDirectoryExist resWithoutExt)
    _ -> return False

  if isDir 
      then return (resWithoutExt </> "main.plm", newScope) 
      else return (res, newScope)

-- | Convert a require expression to an abstract expression
-- | This function is used to load a module and parse it
convertRequire
  :: Translator Error CST.Expression AST.Expression
convertRequire f (CST.ERequire modName) = do
  oldScope <- readIORef mode
  (path, scope) <- getPath (toString modName) Nothing
  
  writeIORef mode scope

  liftIO (doesFileExist path) >>= \case
    False -> do
      pos <- readIORef positionRef
      throwError $ case pos of
        Just p -> ModuleNotFound modName p
        Nothing -> NoPositionSaved
    True -> do
      -- Checking if the module was already parsed to avoid parsing it again
      isAlreadyParsed <- S.member path <$> liftIO (readIORef parsedPaths)
      if isAlreadyParsed
        then bireturn Empty
        else do
          -- Adding the path to the already parsed paths set
          modifyIORef' parsedPaths (S.insert path)

          -- Reading the content of the module file.
          content <- decodeUtf8 @Text <$> readFileBS path

          -- Creating the next module current directory by taking the directory
          -- part of the module path.
          let newCurrentDirectory = takeDirectory path

          -- Fetching imports from the module file
          paths <- liftIO $! fromEither [] <$> parse getPaths path content
          exprs <- translateImports paths newCurrentDirectory f
          writeIORef mode oldScope
        
          res <- parseFile (path, content) newCurrentDirectory

          local (const (newCurrentDirectory, False)) $
            case res of 
              Left err -> throwError err
              Right cst -> sequenceMapM f cst >>= \case
                Left err -> throwError err
                Right ast -> do
                  bireturn . Spread $ exprs <> flat ast

convertRequire _ _ = throwError $ CompilerError "Received invalid require expression"

-- | Translate a list of imports to a list of expressions
translateImports 
  :: [(Text, Maybe CST.Position)]
  -> FilePath
  -> (CST.Expression -> TranslatorReader Error AST.Expression)
  -> IOReader (FilePath, Bool) [AST.Expression]
translateImports paths cwd f = do
  xs <- sequenceMapM (\(path, pos) -> 
    local (const (cwd, True))
      . withMaybePos pos
      $ convertRequire f (CST.ERequire path)) paths
  return . fromEither [] $ flat <$> xs

-- | Parse a file and return the parsed expressions
parseFile 
  :: (FilePath, FileContent)
  -> FilePath
  -> IOReader (FilePath, Bool) (Either Error [CST.Expression])
parseFile (path, content) cwd = do
  local (const (cwd, False)) $ do
    ops <- liftIO $ readIORef operators
    x <- liftIO $! parsePlumeFile path content ops
    case x of
      Left err -> throwError' $ ParserError err path content
      Right (cst, ops') -> do
        modifyIORef' operators (ops' `SL.union`)
        return $ Right cst
