module Language.Plume.Backend.CLang.Monad where

import Language.Plume.Syntax.CLang qualified as CLang
import GHC.IO qualified as IO
import Data.Map qualified as Map

type MonadCLang m = MonadIO m

{-# NOINLINE currentBlock #-}
currentBlock :: IORef (Maybe Text)
currentBlock = IO.unsafePerformIO $ newIORef Nothing

{-# NOINLINE blocks #-}
blocks :: IORef [(Text, [CLang.CLang "expression"])]
blocks = IO.unsafePerformIO $ newIORef []

{-# NOINLINE symbolCounter #-}
symbolCounter :: IORef Int
symbolCounter = IO.unsafePerformIO $ newIORef 0

{-# NOINLINE resultState #-}
resultState :: IORef [CLang.CLang "declaration"]
resultState = IO.unsafePerformIO $ newIORef []

{-# NOINLINE reserved #-}
reserved :: IORef (Set Text)
reserved = IO.unsafePerformIO $ newIORef mempty

freshSymbol :: MonadCLang m => Text -> m Text
freshSymbol n = do
  modifyIORef' symbolCounter (+1)
  i <- readIORef symbolCounter

  pure $ n <> "__clang__" <> show i
  

withVariable :: MonadCLang m => Text -> CLang.CLang "expression" -> m a -> m a
withVariable name e m = do
  old <- readIORef environment
  modifyIORef' environment $ Map.insert name e
  x <- m
  writeIORef environment old
  pure x

{-# NOINLINE environment #-}
environment :: IORef (Map Text (CLang.CLang "expression"))
environment = IO.unsafePerformIO $ newIORef mempty

emitExpr :: MonadCLang m => CLang.CLang "expression" -> m ()
emitExpr e = do
  block <- readIORef currentBlock
  case block of
    Just b -> modifyIORef' blocks $ update b (<> [e])
    Nothing -> pure ()

update :: (Eq a, Monoid b) => a -> (b -> b) -> [(a, b)] -> [(a, b)]
update k f [] = [(k, f mempty)]
update k f ((k', v) : xs) = if k == k' then (k, f v) : xs else (k', v) : update k f xs

emitBlock :: MonadCLang m => Text -> m ()
emitBlock name = do
  modifyIORef' blocks $ update name id
  writeIORef currentBlock (Just name)

emitDecl :: MonadCLang m => CLang.CLang "expression" -> CLang.CLang "type" -> m (CLang.CLang "expression")
emitDecl e t = do
  name <- freshSymbol "tmp"
  emitExpr $ CLang.MkExprLet name t e

  pure $ CLang.MkExprVariable name t
  

reset :: MonadCLang m => m a -> m a
reset m = do
  old <- readIORef blocks
  oldEnv <- readIORef environment
  writeIORef blocks []
  x <- m
  writeIORef blocks old
  writeIORef environment oldEnv
  writeIORef currentBlock Nothing
  pure x