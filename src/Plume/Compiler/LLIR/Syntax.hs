module Plume.Compiler.LLIR.Syntax where

import GHC.Show 
import GHC.Records
import Prelude hiding (show)
import System.IO.Color
import Plume.Compiler.Desugaring.Syntax qualified as Pre
import Plume.Syntax.Abstract (IsStandard)
import Plume.Syntax.Common qualified as Cmm

type NeededLocalSpace = Int

data Instruction 
  = LoadLocal Text
  | StoreLocal Text
  | LoadConstant Int
  | LoadGlobal Text
  | StoreGlobal Text
  | Update
  | Return
  | ReturnConst Int
  | Compare Comparator
  | Add | Sub | AddConst Int | SubConst Int
  | Mul | MulConst Int
  | LoadNative Text
  | MakeList Int
  | ListGet Int
  | Call Int
  | CallGlobal Text Int | CallLocal Text Int
  | JumpElseRel Int
  | JumpElseRelCmp Int Comparator
  | JumpElseRelCmpConstant Int Comparator Int
  | IJumpElseRelCmp Int Comparator
  | IJumpElseRelCmpConstant Int Comparator Int
  | JumpRel Int
  | MakeMutable | UnMut
  | GetIndex
  | Special
  | Slice Int
  | ListLength
  | Halt
  deriving (Eq)

data Segment
  = Function Text [Text] NeededLocalSpace LocalSpace [Instruction]
  | Instruction Instruction
  deriving (Eq)

type NameAddress = Int
type LibraryAddress = Int
type FunctionIndex = Int
type LocalSpace = [(Text, Int)]

data NativeFunction = NativeFunction
  { nativeAddressName :: NameAddress
  , nativeFunctionIndex :: FunctionIndex
  , nativeFunctionLibrary :: LibraryAddress
  } deriving (Show, Eq)

data NativeLibrary = MkNativeLibrary 
  { nativeLibraryPath :: Pre.LibraryPath
  , nativeAddressLibrary :: LibraryAddress
  , nativeIsStandard :: IsStandard
  , nativeFunctions :: Natives
  } deriving (Show, Eq)

type Natives = Map Text NativeFunction

type Libraries = Map Pre.LibraryPath NativeLibrary

type Program = ([Segment], Libraries, [Cmm.Literal])

showConstant :: Int -> String
showConstant n = decorate ("#" <> show n) (Yellow, NoColor, Null)

instance Show Instruction where
  show (LoadLocal name) = "load " <> decorate (toString name) (NoColor, NoColor, Bold)
  show (StoreLocal name) = "store " <> decorate (toString name) (NoColor, NoColor, Bold)
  show (LoadConstant n) = "load " <> showConstant n
  show (LoadGlobal name) = "load " <> decorate (toString name) (Red, NoColor, Bold)
  show (StoreGlobal name) = "store " <> decorate (toString name) (Red, NoColor, Bold)
  show Update = "update"
  show Return = "return"
  show (ReturnConst n) = "return " <> showConstant n
  show (Compare c) = "compare " <> show c
  show Add = "add"
  show Sub = "sub"
  show (AddConst n) = "add " <> showConstant n
  show (SubConst n) = "sub " <> showConstant n
  show Mul = "mul"
  show (MulConst n) = "mul " <> showConstant n
  show (LoadNative name) = "load native " <> decorate (toString name) (Green, NoColor, Bold)
  show (MakeList n) = "make list " <> show n
  show (ListGet n) = "index " <> show n
  show (Call arity) = "call with " <> show arity <> " arguments"
  show (CallGlobal name n) = "call " <> decorate (toString name) (Red, NoColor, Bold) <> " with " <> show n <> " arguments"
  show (CallLocal name n) = "call " <> decorate (toString name) (NoColor, NoColor, Bold) <> " with " <> show n <> " arguments"
  show (JumpElseRel n) = "jump by " <> show n <> " if false"
  show (JumpElseRelCmp n c) = "jump by " <> show c <> " if " <> show n <> " is false"
  show (JumpElseRelCmpConstant n c m) = "jump by " <> show c <> " if " <> show n <> " " <> showConstant m <> " is false"
  show (IJumpElseRelCmp n c) = "jump by " <> show c <> " if " <> show n <> " is true"
  show (IJumpElseRelCmpConstant n c m) = "jump by " <> show c <> " if " <> show n <> " " <> showConstant m <> " is false"
  show (JumpRel n) = "jump by " <> show n
  show MakeMutable = "make mutable"
  show UnMut = "unmut"
  show GetIndex = "get index"
  show Special = "special"
  show (Slice n) = "slice " <> show n
  show ListLength = "list length"
  show Halt = "halt"


instance Show Segment where
  show (Function name args _ _ body) = name' <> "(" <> args' <> ")" <> ":\n" <> body'
    where args' = let args'' = map (flip decorate (Black, NoColor, Null) . toString) args
                  in intercalate ", " args''
          name' = decorate (toString name) (NoColor, NoColor, Bold)
          body' = intercalate "\n" (map (toString . ("  " <>) . show) body)
  show (Instruction i) = show i

data Comparator
  = LessThan
  | GreaterThan
  | EqualTo
  | NotEqualTo
  | LessThanOrEqualTo
  | GreaterThanOrEqualTo
  | AndCmp | OrCmp
  deriving (Eq)

instance Show Comparator where
  show LessThan = "<"
  show GreaterThan = ">"
  show EqualTo = "=="
  show NotEqualTo = "!="
  show LessThanOrEqualTo = "<="
  show GreaterThanOrEqualTo = ">="
  show AndCmp = "&&"
  show OrCmp = "||"

storeGlobal :: Text -> [Segment]
storeGlobal name = [Instruction (StoreGlobal name)]

loadGlobal :: Text -> [Segment]
loadGlobal name = [Instruction (LoadGlobal name)]

storeLocal :: Text -> [Segment]
storeLocal name = [Instruction (StoreLocal name)]

loadLocal :: Text -> [Segment]
loadLocal name = [Instruction (LoadLocal name)]

loadConstant :: Int -> [Segment]
loadConstant n = [Instruction (LoadConstant n)]

instr :: Instruction -> [Segment]
instr = pure . Instruction

deriveHasField ''NativeLibrary
deriveHasField ''NativeFunction