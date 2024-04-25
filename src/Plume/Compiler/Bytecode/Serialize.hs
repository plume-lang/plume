module Plume.Compiler.Bytecode.Serialize where

import Data.Binary
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text.Encoding (encodeUtf8)
import Plume.Compiler.Bytecode.Syntax
import Prelude hiding (encodeUtf8)
import Plume.Syntax.Abstract.Expression (IsStandard)

encodeInteger :: (Integral a) => a -> Put
encodeInteger = putInt32le . fromIntegral

encodeComparator :: Comparator -> Put
encodeComparator LessThan = putInt32le 0
encodeComparator GreaterThan = putInt32le 1
encodeComparator EqualTo = putInt32le 2
encodeComparator NotEqualTo = putInt32le 3
encodeComparator LessThanOrEqualTo = putInt32le 4
encodeComparator GreaterThanOrEqualTo = putInt32le 5
encodeComparator AndCmp = putInt32le 6
encodeComparator OrCmp = putInt32le 7

encodeNull :: Put
encodeNull = putInt32le 0

replicateNull :: Int -> Put
replicateNull = flip replicateM_ encodeNull

encodeInstr :: Int -> Put
encodeInstr = putInt32le . fromIntegral

encodeInstruction :: Instruction -> Put
encodeInstruction (LoadLocal i) = 
  encodeInstr 0 >> encodeInteger i >> replicateNull 2
encodeInstruction (StoreLocal i) = 
  encodeInstr 1 >> encodeInteger i >> replicateNull 2
encodeInstruction (LoadConstant i) = 
  encodeInstr 2 >> encodeInteger i >> replicateNull 2
encodeInstruction (LoadGlobal i) = 
  encodeInstr 3 >> encodeInteger i >> replicateNull 2
encodeInstruction (StoreGlobal i) = 
  encodeInstr 4 >> encodeInteger i >> replicateNull 2
encodeInstruction Return = 
  encodeInstr 5 >> replicateNull 3
encodeInstruction (Compare c) = 
  encodeInstr 6 >> encodeComparator c >> replicateNull 2
encodeInstruction And = 
  encodeInstr 7 >> replicateNull 3
encodeInstruction Or = 
  encodeInstr 8 >> replicateNull 3
encodeInstruction (LoadNative i cp fp) = 
  encodeInstr 9 >> encodeInteger i >> encodeInteger cp >> encodeInteger fp
encodeInstruction (MakeList i) = 
  encodeInstr 10 >> encodeInteger i >> replicateNull 2
encodeInstruction (ListGet i) = 
  encodeInstr 11 >> encodeInteger i >> replicateNull 2
encodeInstruction (Call i) = 
  encodeInstr 12 >> encodeInteger i >> replicateNull 2
encodeInstruction (JumpElseRel i) = 
  encodeInstr 13 >> encodeInteger i >> replicateNull 2
encodeInstruction TypeOf = 
  encodeInstr 14 >> replicateNull 3
encodeInstruction ConstructorName = 
  encodeInstr 15 >> replicateNull 3
encodeInstruction (Phi i j) = 
  encodeInstr 16 >> encodeInteger i >> encodeInteger j >> encodeNull
encodeInstruction (MakeLambda i l) = 
  encodeInstr 17 >> encodeInteger i >> encodeInteger l >> encodeNull
encodeInstruction GetIndex = 
  encodeInstr 18 >> replicateNull 3
encodeInstruction Special = 
  encodeInstr 19 >> replicateNull 3
encodeInstruction (JumpRel i) = 
  encodeInstr 20 >> encodeInteger i >> replicateNull 2
encodeInstruction (Slice i) = 
  encodeInstr 21 >> encodeInteger i >> replicateNull 2
encodeInstruction ListLength = 
  encodeInstr 22 >> replicateNull 3
encodeInstruction Halt = 
  encodeInstr 23 >> replicateNull 3
encodeInstruction Update = 
  encodeInstr 24 >> replicateNull 3
encodeInstruction MakeMutable = 
  encodeInstr 25 >> replicateNull 3
encodeInstruction UnMut = 
  encodeInstr 26 >> replicateNull 3
encodeInstruction Add = 
  encodeInstr 27 >> replicateNull 3
encodeInstruction Sub = 
  encodeInstr 28 >> replicateNull 3
encodeInstruction (ReturnConst i) = 
  encodeInstr 29 >> encodeInteger i >> replicateNull 2
encodeInstruction (AddConst i) = 
  encodeInstr 30 >> encodeInteger i >> replicateNull 2
encodeInstruction (SubConst i) = 
  encodeInstr 31 >> encodeInteger i >> replicateNull 2
encodeInstruction (JumpElseRelCmp i c) =
  encodeInstr 32 >> encodeInteger i >> encodeComparator c >> encodeNull
encodeInstruction (IJumpElseRelCmp i c) =
  encodeInstr 33 >> encodeInteger i >> encodeComparator c >> encodeNull
encodeInstruction (JumpElseRelCmpConstant i c j) =
  encodeInstr 34 >> encodeInteger i >> encodeComparator c >> encodeInteger j
encodeInstruction (IJumpElseRelCmpConstant i c j) =
  encodeInstr 35 >> encodeInteger i >> encodeComparator c >> encodeInteger j
encodeInstruction (CallGlobal i j) =
  encodeInstr 36 >> encodeInteger i >> encodeInteger j >> encodeNull
encodeInstruction (CallLocal i j) =
  encodeInstr 37 >> encodeInteger i >> encodeInteger j >> encodeNull
encodeInstruction (MakeAndStoreLambda i j k) =
  encodeInstr 38 >> encodeInteger i >> encodeInteger j >> encodeInteger k
encodeInstruction Mul =
  encodeInstr 39 >> replicateNull 3
encodeInstruction (MulConst i) =
  encodeInstr 40 >> encodeInteger i >> replicateNull 2

encodeText :: Text -> Put
encodeText w = do
  encodeInteger $ BS.length encoded
  putByteString encoded
 where
  encoded = encodeUtf8 w

encodeConstant :: Constant -> Put
encodeConstant (CInt i) = putWord8 0 >> encodeInteger i
encodeConstant (CFloat f) = putWord8 1 >> putDoublele f
encodeConstant (CString t) = putWord8 2 >> encodeText t

encodeMetaData :: FunctionMetaData -> Put
encodeMetaData FunctionMetaData {arity, address, localsSpace} = do
  encodeInteger arity
  encodeInteger address
  encodeInteger localsSpace

encodeProgram :: Program -> Put
encodeProgram Program {pInstructions, pConstants, pNativeLibraries} = do
  encodeInteger $ length pConstants
  mapM_ encodeConstant pConstants

  encodeInteger $ length pNativeLibraries
  mapM_ encodeNative pNativeLibraries

  encodeInteger $ length pInstructions
  mapM_ encodeInstruction pInstructions

encodeNative :: ((FilePath, IsStandard), Int) -> Put
encodeNative ((path, isStandard), idx) = do
  encodeText $ fromString path
  encodeInteger $ fromEnum isStandard
  encodeInteger idx

serialize :: Program -> IO BSL.ByteString
serialize = pure . runPut . encodeProgram
