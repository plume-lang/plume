module Plume.Compiler.Bytecode.Syntax where
import GHC.Records
import Plume.Syntax.Common.Literal
import qualified Plume.Compiler.Desugaring.Syntax as Pre
import Plume.Compiler.ClosureConversion.Syntax (Update(..))
import qualified Data.List as List
import GHC.IO (unsafePerformIO)
import qualified Data.Map as Map

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
  | Mul | MulConst Int
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
  { pInstructions :: [Instruction]
  , pConstants :: [Constant]
  , pNativeLibraries :: [(FilePath, Int)]
  }

data FunctionMetaData = FunctionMetaData
  { arity :: Int
  , address :: Int
  , localsSpace :: Int
  , localVariables :: Map Int Text
  }
  deriving (Show, Eq)

class Free a where
  free :: a -> [Text]

instance (Free a) => Free [a] where
  free = foldMap free

instance Free Pre.DesugaredExpr where
  free (Pre.DEVar x) = [x]
  free (Pre.DEApplication f args) = [f] <> foldMap free args
  free (Pre.DEList es) = foldMap free es
  free (Pre.DEProperty e _) = free e
  free (Pre.DEIf e1 e2 e3) = free e1 <> free e2 <> free e3
  free (Pre.DETypeOf e) = free e
  free (Pre.DEIsConstructor e _) = free e
  free (Pre.DEEqualsTo e1 e2) = free e1 <> free e2
  free (Pre.DEAnd e1 e2) = free e1 <> free e2
  free (Pre.DEDictionary es) = foldMap free es
  free _ = mempty

instance Free Update where
  free (UVariable x) = [x]
  free (UProperty e _) = free e

instance Free Pre.DesugaredStatement where
  free (Pre.DSExpr e) = free e
  free (Pre.DSReturn e) = free e
  free (Pre.DSDeclaration n e) = [n] <> free e
  free (Pre.DSMutDeclaration n e) = [n] <> free e
  free (Pre.DSMutUpdate n e) = free n <> free e

instance Free Pre.DesugaredProgram where
  free (Pre.DPFunction name _ _) = List.singleton name
  free (Pre.DPStatement s) = free s
  free (Pre.DPNativeFunction {}) = mempty
  free (Pre.DPDeclaration n _) = List.singleton n
  free (Pre.DPMutDeclaration n _) = List.singleton n
  free (Pre.DPMutUpdate n _) = free n

data AssemblerState = AssemblerState
  { constants :: Map Literal Int
  , nativeFunctions :: Map Text (Int, Int, Int)
  , metadata :: IntMap FunctionMetaData
  , currentSize :: Int
  , locals :: Map Text Int
  , globals :: Map Text Int
  , nativeLibraries :: [(FilePath, [Text])]
  , cwd :: FilePath
  }
  deriving (Show, Eq)

deriveHasField ''AssemblerState

{-# NOINLINE assemblerState #-}
assemblerState :: IORef AssemblerState
assemblerState =
  unsafePerformIO $
    newIORef $
      AssemblerState mempty mempty mempty 0 mempty mempty mempty mempty

assembleLit :: Literal -> IO Int
assembleLit l = do
  AssemblerState {constants} <- readIORef assemblerState
  case Map.lookup l constants of
    Just i' -> pure i'
    Nothing -> do
      modifyIORef' assemblerState $ \s ->
        s {constants = Map.insert l (Map.size constants) constants}
      let idx = Map.size constants
      pure idx