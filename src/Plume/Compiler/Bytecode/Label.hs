module Plume.Compiler.Bytecode.Label where

import GHC.IO qualified as IO
import Plume.Compiler.LLIR.Syntax qualified as LLIR
import Data.Map qualified as Map
import Plume.Compiler.Bytecode.Syntax (LabelPool, Label(..))

{-# NOINLINE labelPool #-}
labelPool :: IORef LabelPool
labelPool = IO.unsafePerformIO $ newIORef Map.empty

{-# NOINLINE labelCounter #-}
labelCounter :: IORef Int
labelCounter = IO.unsafePerformIO $ newIORef 0

createNewAddress :: Text -> Label -> IO ()
createNewAddress label lb = modifyIORef' labelPool (Map.insert label lb)

fetchLabel :: Text -> IO Label
fetchLabel label = do
  pool <- readIORef labelPool
  case Map.lookup label pool of
    Just lb -> pure lb
    Nothing -> error "Label not found"

{-# NOINLINE currentAddress #-}
currentAddress :: IORef Int
currentAddress = IO.unsafePerformIO $ newIORef 0

incrementAddressBy :: Int -> IO ()
incrementAddressBy n = modifyIORef' currentAddress (+n)

incrementAddress :: IO ()
incrementAddress = incrementAddressBy 1

class Unlabelize a where
  unlabelize :: a -> IO a

instance Unlabelize a => Unlabelize [a] where
  unlabelize = mapM unlabelize

instance Unlabelize LLIR.Segment where
  unlabelize (LLIR.Function name args ls lsSp body) = do
    addr <- readIORef currentAddress
    let lb = MkLabel name addr ls
    createNewAddress name lb
    incrementAddress
    LLIR.Function name args ls lsSp <$> unlabelize body

  unlabelize (LLIR.Instruction instr) = do
    incrementAddress
    return $ LLIR.Instruction instr

instance Unlabelize LLIR.Instruction where
  unlabelize x = incrementAddress $> x

runUnlabelize :: Unlabelize a => a -> IO (a, LabelPool)
runUnlabelize x = do
  writeIORef currentAddress 0
  writeIORef labelPool Map.empty

  x' <- unlabelize x
  
  pool <- readIORef labelPool
  pure (x', pool)