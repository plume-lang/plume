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
encodeInstruction (CLoad i) = putWord8 2 >> encodeInteger i
encodeInstruction (FLoad i) = putWord8 3 >> encodeInteger i
encodeInstruction (LoadGlobal i) = putWord8 4 >> encodeInteger i
encodeInstruction (StoreGlobal i) = putWord8 5 >> encodeInteger i
encodeInstruction Return = putWord8 6
encodeInstruction (Compare c) = putWord8 7 >> encodeComparator c
encodeInstruction And = putWord8 8
encodeInstruction Or = putWord8 9
encodeInstruction (NLoad i) = putWord8 10 >> encodeInteger i
encodeInstruction (MakeList i) = putWord8 11 >> encodeInteger i
encodeInstruction (ListGet i) = putWord8 12 >> encodeInteger i
encodeInstruction (Call i) = putWord8 13 >> encodeInteger i
encodeInstruction (Jump i) = putWord8 14 >> encodeInteger i
encodeInstruction (JumpIf i) = putWord8 15 >> encodeInteger i
encodeInstruction (JumpIfRel i) = putWord8 16 >> encodeInteger i
encodeInstruction TypeOf = putWord8 17
encodeInstruction ConstructorName = putWord8 18
encodeInstruction (Phi i j) = putWord8 19 >> encodeInteger i >> encodeInteger j
encodeInstruction (MakeLambda i) = putWord8 20 >> encodeInteger i

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
encodeProgram Program {instructions, constants, metaDatas} = do
  encodeInteger $ length instructions
  mapM_ encodeInstruction instructions
  encodeInteger $ length constants
  mapM_ encodeConstant constants
  encodeInteger $ length metaDatas
  mapM_ encodeMetaData metaDatas

serialize :: Program -> IO BSL.ByteString
serialize = pure . runPut . encodeProgram