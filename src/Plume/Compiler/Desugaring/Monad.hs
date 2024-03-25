module Plume.Compiler.Desugaring.Monad where

import GHC.IO
import Plume.Compiler.Desugaring.Syntax

type MonadDesugar b = IO b
type DesugarModule a b = Desugar a b -> Desugar a b
type Desugar a b = a -> MonadDesugar b

type ANFResult a = (a, [DesugaredStatement])

return' :: a -> MonadDesugar (ANFResult a)
return' x = return (x, [])

{-# NOINLINE nameCounter #-}
nameCounter :: IORef Int
nameCounter = unsafePerformIO $ newIORef 0

freshName :: (MonadIO m) => m Text
freshName = do
  i <- readIORef nameCounter
  writeIORef nameCounter (i + 1)
  pure $ "var" <> show i

{-# NOINLINE nativeFunctions #-}
nativeFunctions :: IORef (Set Text)
nativeFunctions = unsafePerformIO $ newIORef mempty

createBlock :: [ANFResult (Maybe DesugaredStatement)] -> [DesugaredStatement]
createBlock ((Just x, stmts) : xss) = stmts ++ x : createBlock xss
createBlock ((Nothing, stmts) : xss) = stmts ++ createBlock xss
createBlock [] = []