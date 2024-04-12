{-# LANGUAGE LambdaCase #-}

module Plume.Syntax.Translation.ConcreteToAbstract.Require where

import Control.Monad.Exception
import Control.Monad.Parser
import Data.Text qualified as T
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

{-# NOINLINE parsedPaths #-}
parsedPaths :: IORef (Set FilePath)
parsedPaths = unsafePerformIO $ newIORef mempty

fromEither :: a -> Either b a -> a
fromEither _ (Right a) = a
fromEither a _ = a

getPath :: Text -> IOReader (FilePath, Bool) FilePath
getPath modName = do
  cwd <- asks fst
  let strModName = toString modName -<.> "plm"
  let isStd = "std:" `T.isPrefixOf` modName
  let modPath =
        if isStd
          then do
            p <- liftIO $ readIORef stdPath
            case p of
              Just p' -> return $ Right (p' </> drop 4 strModName)
              Nothing -> throwError' $ CompilerError "Standard library path not set"
          else return $ Right $ cwd </> strModName
  modPath `with` liftIO . absolutize


convertRequire
  :: Translator Error CST.Expression AST.Expression
convertRequire f (CST.ERequire modName) = do
  path <- getPath modName
  liftIO (doesFileExist path) >>= \case
    False -> do
      pos <- readIORef positionRef
      throwError $ case pos of
        Just p -> ModuleNotFound modName p
        Nothing -> NoPositionSaved
    True -> do
      isAlreadyParsed <- S.member path <$> liftIO (readIORef parsedPaths)
      if isAlreadyParsed
        then bireturn Empty
        else do
          modifyIORef' parsedPaths (S.insert path)

          -- Reading the content of the module file.
          content <- decodeUtf8 @Text <$> readFileBS path

          -- Creating the next module current directory by taking the directory
          -- part of the module path.
          let newCurrentDirectory = takeDirectory path

          paths <- liftIO $! fromEither [] <$> parse getPaths path content
          imports' <-
            sequenceMapM
              ( \(i, p) ->
                  local (const (newCurrentDirectory, True))
                    . withMaybePos p
                    $ convertRequire f (CST.ERequire i)
              )
              paths

          let exprs = fromEither [] $ flat <$> imports'

          -- Parsing the module file and converting it to an abstract syntax tree.
          -- We need to use the local function to change the current directory
          -- without globally changing it.
          -- Returning the generated AST as a spreadable AST (just a list of
          -- expressions represented as a single expression).
          local (const (newCurrentDirectory, False)) $ do
            ops <- liftIO $ readIORef operators
            x <- liftIO $! parsePlumeFile path content ops
            case x of
              Left err -> throwError $ ParserError err
              Right (cst, ops') -> do
                modifyIORef' operators (ops' `SL.union`)
                sequenceMapM f cst >>= \case
                  Left err -> throwError err
                  Right ast -> bireturn . Spread $ exprs <> flat ast
convertRequire _ _ = throwError $ CompilerError "Received invalid require expression"

sequenceMapM
  :: (Monad m, Traversable t, Monad f)
  => (a -> f (m a1))
  -> t a
  -> f (m (t a1))
sequenceMapM f = (sequence <$>) . mapM f
