module Plume.Compiler.Bytecode.Syntax where

data Instruction
  = LoadLocal Int
  | StoreLocal Int
  | LoadConstant Int
  | LoadGlobal Int
  | StoreGlobal Int
  | Return
  | Compare Comparator
  | And
  | Or
  | NLoad Int
  | MakeList Int
  | ListGet Int
  | Call Int
  | JumpIfRel Int
  | TypeOf
  | ConstructorName
  | Phi Int Int
  | MakeLambda Int Int
  | GetIndex
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
  , metaDatas :: [FunctionMetaData]
  }

data FunctionMetaData = FunctionMetaData
  { arity :: Int
  , address :: Int
  , localsSpace :: Int
  }
  deriving (Show, Eq)