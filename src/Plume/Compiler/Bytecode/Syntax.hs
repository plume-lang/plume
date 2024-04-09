module Plume.Compiler.Bytecode.Syntax where

data Instruction
  = LoadLocal Int
  | StoreLocal Int
  | LoadConstant Int
  | LoadGlobal Int
  | StoreGlobal Int
  | UpdateGlobal Int
  | UpdateLocal Int
  | Return
  | Compare Comparator
  | And
  | Or
  | LoadNative Int Int Int
  | MakeList Int
  | ListGet Int
  | Call Int
  | JumpIfRel Int
  | JumpRel Int
  | TypeOf
  | ConstructorName
  | Phi Int Int
  | MakeLambda Int Int
  | MakeMutable
  | UnMut
  | GetIndex
  | Special
  | Slice Int
  | ListLength
  | Halt
  deriving (Show, Eq)

data Comparator
  = LessThan
  | GreaterThan
  | EqualTo
  | NotEqualTo
  | LessThanOrEqualTo
  | GreaterThanOrEqualTo
  deriving (Show, Eq)

data Constant
  = CInt Int
  | CFloat Double
  | CString Text
  deriving (Show, Eq)

data Program = Program
  { instructions :: [Instruction]
  , constants :: [Constant]
  , nativeLibraries :: [(FilePath, Int)]
  }

data FunctionMetaData = FunctionMetaData
  { arity :: Int
  , address :: Int
  , localsSpace :: Int
  , locals :: Map Int Text
  }
  deriving (Show, Eq)