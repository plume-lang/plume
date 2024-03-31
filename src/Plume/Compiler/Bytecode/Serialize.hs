module Plume.Compiler.Bytecode.Serialize where

import Data.Binary
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text.Encoding (encodeUtf8)
import Plume.Compiler.Bytecode.Syntax
import Prelude hiding (encodeUtf8)

encodeInteger :: (Integral a) => a -> Put
encodeInteger = putInt64le . fromIntegral

encodeComparator :: Comparator -> Put
encodeComparator LessThan = putInt64le 0
encodeComparator GreaterThan = putInt64le 1
encodeComparator EqualTo = putInt64le 2
encodeComparator NotEqualTo = putInt64le 3
encodeComparator LessThanOrEqualTo = putInt64le 4
encodeComparator GreaterThanOrEqualTo = putInt64le 5

encodeInstruction :: Instruction -> Put
encodeInstruction (LoadLocal i) = putWord8 0 >> encodeInteger i
encodeInstruction (StoreLocal i) = putWord8 1 >> encodeInteger i
encodeInstruction (LoadConstant i) = putWord8 2 >> encodeInteger i
encodeInstruction (LoadGlobal i) = putWord8 3 >> encodeInteger i
encodeInstruction (StoreGlobal i) = putWord8 4 >> encodeInteger i
encodeInstruction Return = putWord8 5
encodeInstruction (Compare c) = putWord8 6 >> encodeComparator c
encodeInstruction And = putWord8 7
encodeInstruction Or = putWord8 8
encodeInstruction (LoadNative i cp fp) = putWord8 9 >> encodeInteger i >> encodeInteger cp >> encodeInteger fp
encodeInstruction (MakeList i) = putWord8 10 >> encodeInteger i
encodeInstruction (ListGet i) = putWord8 11 >> encodeInteger i
encodeInstruction (Call i) = putWord8 12 >> encodeInteger i
encodeInstruction (JumpIfRel i) = putWord8 13 >> encodeInteger i
encodeInstruction TypeOf = putWord8 14
encodeInstruction ConstructorName = putWord8 15
encodeInstruction (Phi i j) = putWord8 16 >> encodeInteger i >> encodeInteger j
encodeInstruction (MakeLambda i l) = putWord8 17 >> encodeInteger i >> encodeInteger l
encodeInstruction GetIndex = putWord8 18
encodeInstruction Special = putWord8 19
encodeInstruction (JumpRel i) = putWord8 20 >> encodeInteger i
encodeInstruction (Slice i) = putWord8 21 >> encodeInteger i
encodeInstruction ListLength = putWord8 22

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
encodeProgram Program {instructions, constants, nativeLibraries} = do
  encodeInteger $ length instructions
  mapM_ encodeInstruction instructions

  encodeInteger $ length constants
  mapM_ encodeConstant constants

  encodeInteger $ length nativeLibraries
  mapM_ encodeNative nativeLibraries

encodeNative :: (FilePath, Int) -> Put
encodeNative (path, idx) = do
  encodeText $ fromString path
  encodeInteger idx

serialize :: Program -> IO BSL.ByteString
serialize = pure . runPut . encodeProgram