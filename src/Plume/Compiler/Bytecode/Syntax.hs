module Plume.Compiler.Bytecode.Syntax where

import Plume.Compiler.LLIR.Syntax qualified as LLIR
import Plume.Compiler.LLIR.Free qualified as LLIR
import Data.Set qualified as Set

data Label = MkLabel 
  { labelName :: Text
  , labelAddress :: Int
  , labelLocalSpace :: LocalSpace
  }
  deriving (Eq, Show)

type LabelPool = Map Text Label

type Address = Int
type NameAddress = Int
type LibraryAddress = Int
type FunctionIndex = Int
type ConstantAddress = Int
type Index = Int
type Size = Int
type FunctionArity = Int
type LocalSpace = Int

data Instruction
  = LoadLocal Address
  | StoreLocal Address
  | LoadConstant ConstantAddress
  | LoadGlobal Address
  | StoreGlobal Address
  | LoadNative NameAddress LibraryAddress FunctionIndex
  | Update
  | Return
  | ReturnConst ConstantAddress
  | Compare LLIR.Comparator
  | Add | Sub | AddConst ConstantAddress | SubConst ConstantAddress
  | Mul | MulConst ConstantAddress
  | MakeList Size
  | ListGet Index

  -- Calling functions
  | Call FunctionIndex
  | CallGlobal FunctionIndex FunctionArity 
  | CallLocal FunctionIndex FunctionArity

  | JumpElseRel Address
  | JumpElseRelCmp Address LLIR.Comparator
  | JumpElseRelCmpConstant Address LLIR.Comparator ConstantAddress
  | IJumpElseRelCmp Address LLIR.Comparator
  | IJumpElseRelCmpConstant Address LLIR.Comparator ConstantAddress
  | JumpRel Address
  | MakeMutable | UnMut
  | MakeLambda Address LocalSpace
  | MakeAndStoreLambda Address Size LocalSpace
  | GetIndex
  | Special
  | Slice Index
  | ListLength
  | Halt
  | ReturnUnit
  | DropGlobal Address Int
  | DropLocal Address Int
  deriving (Show, Eq)

instance LLIR.Free LLIR.Segment where
  free _ (LLIR.Function name _ _ _ _) = Set.singleton name
  free _ (LLIR.Instruction (LLIR.StoreGlobal n)) = Set.singleton n
  free _ _ = mempty
