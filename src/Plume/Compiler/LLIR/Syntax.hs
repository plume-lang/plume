module Plume.Compiler.LLIR.Syntax where

import GHC.Show 
import Prelude hiding (show)
import System.IO.Color

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
  | Call Text
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
  = Function Text [Text] [Instruction]
  | Instruction Instruction
  deriving (Eq)

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
  show (Call name) = "call " <> decorate (toString name) (Cyan, NoColor, Bold) 
  show (CallGlobal name n) = "call " <> decorate (toString name) (Cyan, NoColor, Bold) <> " with " <> show n <> " arguments"
  show (CallLocal name n) = "call " <> decorate (toString name) (Cyan, NoColor, Bold) <> " with " <> show n <> " arguments"
  show (JumpElseRel n) = "jump if not " <> show n
  show (JumpElseRelCmp n c) = "jump if not " <> show c <> " " <> show n
  show (JumpElseRelCmpConstant n c m) = "jump if not " <> show c <> " " <> show n <> " " <> showConstant m
  show (IJumpElseRelCmp n c) = "jump if " <> show c <> " " <> show n
  show (IJumpElseRelCmpConstant n c m) = "jump if " <> show c <> " " <> show n <> " " <> showConstant m
  show (JumpRel n) = "jump " <> show n
  show MakeMutable = "make mutable"
  show UnMut = "unmut"
  show GetIndex = "get index"
  show Special = "special"
  show (Slice n) = "slice " <> show n
  show ListLength = "list length"
  show Halt = "halt"


instance Show Segment where
  show (Function name args body) = name' <> "(" <> args' <> ")" <> ":\n" <> body'
    where args' = let args'' = map (flip decorate (Black, NoColor, Null) . toString) args
                  in intercalate ", " args''
          name' = decorate (toString name) (NoColor, NoColor, Bold)
          body' = intercalate "\n" (map (toString . ("  " <>) . show) body)
  show (Instruction i) = show i

type Program = [Segment]

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

storeGlobal :: Text -> Program
storeGlobal name = [Instruction (StoreGlobal name)]

loadGlobal :: Text -> Program
loadGlobal name = [Instruction (LoadGlobal name)]

storeLocal :: Text -> Program
storeLocal name = [Instruction (StoreLocal name)]

loadLocal :: Text -> Program
loadLocal name = [Instruction (LoadLocal name)]

loadConstant :: Int -> Program
loadConstant n = [Instruction (LoadConstant n)]
