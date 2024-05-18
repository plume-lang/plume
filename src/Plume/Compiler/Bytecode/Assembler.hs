module Plume.Compiler.Bytecode.Assembler where

import GHC.IO qualified as IO
import Plume.Compiler.Bytecode.Syntax qualified as BC
import Plume.Compiler.LLIR.Syntax qualified as LLIR
import Plume.Compiler.LLIR.Free qualified as LLIR
import Data.Map qualified as Map
import Data.Set qualified as Set
import Control.Monad.IO

{-# NOINLINE standardPath #-}
standardPath :: IORef Text
standardPath = IO.unsafePerformIO $ newIORef ""

{-# NOINLINE nativeFunctions #-}
nativeFunctions :: IORef (Map Text LLIR.NativeFunction)
nativeFunctions = IO.unsafePerformIO $ newIORef mempty

{-# NOINLINE localPool #-}
localPool :: IORef (Map Text Int)
localPool = IO.unsafePerformIO $ newIORef mempty

{-# NOINLINE globalPool #-}
globalPool :: IORef (Map Text Int)
globalPool = IO.unsafePerformIO $ newIORef mempty

class Assemble a where
  assemble :: a -> IOReader BC.LabelPool [BC.Instruction]

instance Assemble a => Assemble [a] where
  assemble = fmap concat . mapM assemble

instance Assemble LLIR.Instruction where
  assemble (LLIR.LoadLocal name) = do
    locals <- readIORef localPool
    case Map.lookup name locals of
      Just address -> do
        address' <- negIdx address
        pure [BC.LoadLocal address']
      Nothing -> error $ "Local " <> name <> " not found"
  
  assemble (LLIR.StoreLocal name) = do
    locals <- readIORef localPool
    case Map.lookup name locals of
      Just address -> do
        address' <- negIdx address
        pure [BC.StoreLocal address']
      Nothing -> error $ "Local " <> name <> " not found"
  
  assemble (LLIR.LoadConstant address) = pure [BC.LoadConstant address]
  
  assemble (LLIR.LoadGlobal address) = do
    globals <- readIORef globalPool
    case Map.lookup address globals of
      Just address' -> pure [BC.LoadGlobal address']
      Nothing -> error $ "Global " <> address <> " not found"
  
  assemble (LLIR.StoreGlobal address) = do
    globals <- readIORef globalPool
    case Map.lookup address globals of
      Just address' -> pure [BC.StoreGlobal address']
      Nothing -> error $ "Global " <> address <> " not found"
  
  assemble (LLIR.LoadNative name) = do
    natives <- readIORef nativeFunctions
    case Map.lookup name natives of
      Just (LLIR.NativeFunction nameAddr funIdx libAddr) -> 
        pure [BC.LoadNative nameAddr libAddr funIdx]
      Nothing -> error $ "Native " <> name <> " not found"
  
  assemble LLIR.Update = pure [BC.Update]
  assemble LLIR.Return = pure [BC.Return]
  assemble (LLIR.ReturnConst address) = pure [BC.ReturnConst address]
  assemble (LLIR.Compare cmp) = pure [BC.Compare cmp]
  assemble LLIR.Add = pure [BC.Add]
  assemble LLIR.Sub = pure [BC.Sub]
  assemble (LLIR.AddConst address) = pure [BC.AddConst address]
  assemble (LLIR.SubConst address) = pure [BC.SubConst address]
  assemble LLIR.Mul = pure [BC.Mul]
  assemble (LLIR.MulConst address) = pure [BC.MulConst address]
  assemble (LLIR.MakeList size) = pure [BC.MakeList size]
  assemble (LLIR.ListGet index) = pure [BC.ListGet index]
  assemble (LLIR.Call arity) = pure [BC.Call arity]
  assemble (LLIR.CallGlobal index arity) = do
    globals <- readIORef globalPool
    case Map.lookup index globals of
      Just idx -> pure [BC.CallGlobal idx arity]
      Nothing -> error $ "Global " <> index <> " not found"
  assemble (LLIR.CallLocal index arity) = do
    locals <- readIORef localPool
    case Map.lookup index locals of
      Just idx -> do
        addr <- negIdx idx
        pure [BC.CallLocal addr arity]
      Nothing -> error $ "Local " <> index <> " not found"
  assemble (LLIR.JumpElseRel address) = pure [BC.JumpElseRel address]
  assemble (LLIR.JumpElseRelCmp address cmp) = pure [BC.JumpElseRelCmp address cmp]
  assemble (LLIR.JumpElseRelCmpConstant address cmp constant) = pure [BC.JumpElseRelCmpConstant address cmp constant]
  assemble (LLIR.IJumpElseRelCmp address cmp) = pure [BC.IJumpElseRelCmp address cmp]
  assemble (LLIR.IJumpElseRelCmpConstant address cmp constant) = pure [BC.IJumpElseRelCmpConstant address cmp constant]
  assemble (LLIR.JumpRel address) = pure [BC.JumpRel address]
  assemble LLIR.MakeMutable = pure [BC.MakeMutable]
  assemble LLIR.UnMut = pure [BC.UnMut]
  assemble LLIR.GetIndex = pure [BC.GetIndex]
  assemble LLIR.Special = pure [BC.Special]
  assemble (LLIR.Slice index) = pure [BC.Slice index]
  assemble LLIR.ListLength = pure [BC.ListLength]
  assemble LLIR.Halt = pure [BC.Halt]
  assemble LLIR.ReturnUnit = pure [BC.ReturnUnit]

instance Assemble LLIR.Segment where
  assemble (LLIR.Function name _ ls freed instructions) = do
    labelPool <- ask
    let freed' = Map.fromList freed

    case Map.lookup name labelPool of
      Just (BC.MkLabel name' _ ls') 
        | name' == name && ls' == ls -> do  
          globals <- readIORef globalPool
          case Map.lookup name globals of
            Just addr -> do
              instructions' <- withLocals freed' $ assemble instructions
              pure (BC.MakeAndStoreLambda addr (length instructions') ls : instructions')
            
            _ -> error $ "Function not found: " <> name
      _ -> error $ "Function not found: " <> name

  assemble (LLIR.Instruction instruction) = assemble instruction

withLocals :: Map Text Int -> IOReader BC.LabelPool a -> IOReader BC.LabelPool a
withLocals locals act = do
  localPool' <- readIORef localPool
  writeIORef localPool locals
  r <- act
  writeIORef localPool localPool'
  pure r

fromMap :: Ord k => Map k a -> Set k
fromMap = Set.fromList . Map.keys

runBytecodeAssembler :: (Assemble a, LLIR.Free a) => (BC.LabelPool, Map Text LLIR.NativeFunction) -> a -> IO [BC.Instruction]
runBytecodeAssembler (lp, nf) x = do
  let globals = Map.fromList . (`zip` [0..]) . Set.toList $ fromMap lp <> LLIR.free mempty x

  writeIORef localPool Map.empty
  writeIORef globalPool globals
  writeIORef nativeFunctions nf

  (<> [BC.Halt]) <$> runReaderT (assemble x) lp 

negIdx :: MonadIO m => Int -> m Int
negIdx i = withRef localPool $ \locals -> negate (Map.size locals) + i