module Plume.Compiler.Bytecode.Serialize where

import Data.Binary
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text.Encoding (encodeUtf8)
import Data.Map qualified as Map
import Plume.Compiler.Bytecode.Syntax
import Prelude hiding (encodeUtf8)
import Plume.Syntax.Common.Literal (Literal(..))
import Plume.Compiler.LLIR.Syntax (Comparator(..), Libraries, NativeLibrary(..))

-- | BYTECODE SERIALIZATION
-- | Bytecode serialization is the process of transforming the bytecode AST into
-- | a binary format that can be read by the virtual machine.
-- |
-- | The serialization process is done in multiple steps:
-- |
-- |  - Encode integer: This step encodes an integer into a binary format.
-- |
-- |  - Encode comparator: This step encodes a comparator into a binary format.
-- |
-- |  - Encode n null(s): This step encodes n null(s) value(s) into a 
-- |    binary format.
-- |
-- |  - Encode instruction: This step encodes an instruction into a binary
-- |    format. An instruction is just an int32 value.
-- |
-- |  - Encode text: This step encodes a text value into a binary format.
-- |
-- |  - Encode constant: This step encodes a constant value into a binary
-- |    format.
-- |
-- |  - Encode native: This step encodes a native library into a binary format.
-- |
-- |  - Encode program: This step encodes a program into a binary format.

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
encodeInstruction ReturnUnit =
  encodeInstr 41 >> replicateNull 3

encodeText :: Text -> Put
encodeText w = do
  encodeInteger $ BS.length encoded
  putByteString encoded
 where
  encoded = encodeUtf8 w

encodeConstant :: Literal -> Put
encodeConstant (LInt i) = putWord8 0 >> encodeInteger i
encodeConstant (LFloat f) = putWord8 1 >> putDoublele f
encodeConstant (LString t) = putWord8 2 >> encodeText t
encodeConstant (LBool b) = putWord8 0 >> encodeInteger (fromEnum b)
encodeConstant (LChar c) = putWord8 2 >> encodeText (fromString [c])

type Program = ([Instruction], Libraries, [Literal])

encodeProgram :: Program -> Put
encodeProgram (xs, libs, lits) = do
  encodeInteger $ length lits
  mapM_ encodeConstant lits

  let libs' = prepareLibs libs
  encodeInteger $ length libs'
  mapM_ encodeNative libs'

  encodeInteger $ length xs
  mapM_ encodeInstruction xs

prepareLibs :: Libraries -> [(Text, Maybe Text, [Text])]
prepareLibs m = do
  let (_, ls) = sequence $ Map.toList m
      ls'     = sortBy (compare `on` (\(MkNativeLibrary _ i _ _) -> i)) ls

  map (\(MkNativeLibrary p _ s n) -> (p, s, Map.keys n)) ls'

encodeType :: Maybe Text -> Put
encodeType (Just "standard") = putWord8 1
encodeType (Just "module") = putWord8 2
encodeType _ = putWord8 0

encodeNative :: (Text, Maybe Text, [Text]) -> Put
encodeNative (path, isStandard, nats) = do
  encodeText path
  encodeType isStandard
  encodeInteger (length nats)

serialize :: Program -> IO BSL.ByteString
serialize = pure . runPut . encodeProgram
