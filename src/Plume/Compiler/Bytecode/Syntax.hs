module Plume.Compiler.Bytecode.Syntax where

data Instruction
  = LoadLocal Int
  | StoreLocal Int
  | LoadConstant Int
  | LoadGlobal Int
  | StoreGlobal Int
  | Update
  | Return
  | ReturnConst Int
  | Compare Comparator
  | And | Or
  | Add | Sub | AddConst Int | SubConst Int
  | LoadNative Int Int Int
  | MakeList Int
  | ListGet Int
  | Call Int
  | CallGlobal Int Int | CallLocal Int Int
  | JumpElseRel Int
  | JumpElseRelCmp Int Comparator
  | JumpElseRelCmpConstant Int Comparator Int
  | IJumpElseRelCmp Int Comparator
  | IJumpElseRelCmpConstant Int Comparator Int
  | JumpRel Int
  | TypeOf
  | ConstructorName
  | Phi Int Int
  | MakeLambda Int Int
  | MakeAndStoreLambda Int Int Int
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
  | AndCmp | OrCmp
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
