{-# LANGUAGE LambdaCase #-}

module Plume.Syntax.Translation.ConcreteToAbstract.Require where

import Control.Monad.Exception
import Control.Monad.Parser
import Data.Text qualified as T
import GHC.IO hiding (liftIO)
import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Concrete qualified as CST
import Plume.Syntax.Parser
import Plume.Syntax.Parser.Modules.ParseImports
import Plume.Syntax.Translation.Generics
import System.Directory
import System.FilePath

{-# NOINLINE parsedPaths #-}
parsedPaths :: IORef [FilePath]
parsedPaths = unsafePerformIO $ newIORef []

fromEither :: a -> Either b a -> a
fromEither _ (Right a) = a
fromEither a _ = a

convertRequire
  :: Translator Error CST.Expression AST.Expression
convertRequire f (CST.ERequire modName) = do
  -- Retrieve the current working directory to use it as a base for the
  -- module path.
  cwd <- asks fst

  -- Creating the absolute module path by joining the current working
  -- directory with the module name.
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

  modPath `with` \path -> do
    liftIO (doesFileExist path) >>= \case
      False -> do
        pos <- readIORef positionRef
        throwError $ case pos of
          Just p -> ModuleNotFound modName p
          Nothing -> NoPositionSaved
      True -> do
        isAlreadyParsed <- elem path <$> liftIO (readIORef parsedPaths)
        if isAlreadyParsed
          then bireturn Empty
          else do
            modifyIORef' parsedPaths (path :)

            -- Reading the content of the module file.
            content <- decodeUtf8 @Text <$> readFileBS path

            -- Creating the next module current directory by taking the directory
            -- part of the module path.
            let newCurrentDirectory = takeDirectory path

            paths <- liftIO $ fromEither [] <$> parse getPaths path content
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
              liftIO (parsePlumeFile path content ops) >>= \case
                Left err -> throwError $ ParserError err
                Right (cst, ops') -> do
                  modifyIORef' operators (ops' ++)
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
