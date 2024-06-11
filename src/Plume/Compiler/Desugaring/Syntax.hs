module Plume.Compiler.Desugaring.Syntax where

import Plume.Compiler.ClosureConversion.Free
import Plume.Compiler.ClosureConversion.Syntax (Update(..))
import Plume.Syntax.Common.Literal
import Plume.Syntax.Abstract (IsStandard)
import GHC.Show
import Prelude hiding (show)
import Data.IntMap qualified as IntMap

data DesugaredExpr
  = DEVar Text
  | DEApplication Text [DesugaredExpr]
  | DELiteral Literal
  | DEList [DesugaredExpr]
  | DEIndex DesugaredExpr DesugaredExpr
  | DEProperty DesugaredExpr Int
  | DEDictionary (IntMap DesugaredExpr)
  | DEIf DesugaredExpr [DesugaredStatement] [DesugaredStatement]
  | DETypeOf DesugaredExpr
  | DEIsConstructor DesugaredExpr Text
  | DEEqualsTo DesugaredExpr DesugaredExpr
  | DEAnd DesugaredExpr DesugaredExpr
  | DESpecial
  | DESlice DesugaredExpr Int
  | DEGreaterThan DesugaredExpr Int
  | DEListLength DesugaredExpr
  | DEUnMut DesugaredExpr
  deriving (Eq, Ord)
  

data DesugaredStatement
  = DSExpr DesugaredExpr
  | DSReturn DesugaredExpr
  | DSDeclaration Text DesugaredExpr
  | DSMutDeclaration Text DesugaredExpr
  | DSMutUpdate Update DesugaredExpr
  | DSIf DesugaredExpr [DesugaredStatement] [DesugaredStatement]
  deriving (Eq, Ord)

type LibraryPath = Text
type FunctionName = Text
type Argument = Text
type FunctionArity = Int

data DesugaredProgram
  = DPFunction FunctionName [Argument] [DesugaredStatement] Bool
  | DPStatement DesugaredStatement
  | DPDeclaration Text DesugaredExpr
  | DPNativeFunction LibraryPath FunctionName FunctionArity IsStandard
  | DPMutDeclaration Text DesugaredExpr
  | DPMutUpdate Update DesugaredExpr
  | DPDeclare Text
  deriving (Eq, Ord)

instance Show DesugaredExpr where
  show (DEVar x) = toString x
  show (DEApplication f args) =
    toString f <> "(" <> intercalate ", " (map show args) <> ")"
  show (DELiteral l) = show l
  show (DEList es) = "[" <> intercalate ", " (map show es) <> "]"
  show (DEIndex e1 e2) = show e1 <> ".index(" <> show e2 <> ")"
  show (DEProperty e i) = show e <> "." <> show i
  show (DEIf e1 e2 e3) =
    "if " <> show e1 <> " then " <> show e2 <> " else " <> show e3
  show (DETypeOf e) = "typeof " <> show e
  show (DEIsConstructor e t) = "isConstructor " <> show e <> " " <> toString t
  show (DEEqualsTo e1 e2) = show e1 <> " == " <> show e2
  show (DEAnd e1 e2) = show e1 <> " && " <> show e2
  show DESpecial = "special"
  show (DESlice e i) = show e <> "[" <> show i <> "]"
  show (DEGreaterThan e i) = show e <> " > " <> show i
  show (DEListLength e) = "length " <> show e
  show (DEUnMut e) = "unmut " <> show e
  show (DEDictionary es) =
    "{" <> intercalate ", " (map showMap (IntMap.toList es)) <> "}"
    where showMap (i, e) = show i <> ": " <> show e

instance Show DesugaredStatement where
  show (DSExpr e) = show e
  show (DSReturn e) = "return " <> show e
  show (DSDeclaration n e) = "let " <> toString n <> " = " <> show e
  show (DSMutDeclaration n e) = "mut " <> toString n <> " = " <> show e
  show (DSMutUpdate n e) = show n <> " = " <> show e
  show (DSIf e1 e2 e3) =
    "sif (" <> show e1 <> ") then " <> intercalate ", " (map show e2)
      <> " else "
      <> intercalate ", " (map show e3)

instance Show DesugaredProgram where
  show (DPFunction name args body _) =
    "function " <> toString name <> "(" <> intercalate ", " (map toString args) <> ") {\n"
      <> intercalate "\n" (map (("  " <>) . show) body)
      <> "\n}"
  show (DPStatement s) = show s
  show (DPDeclaration n e) = "let " <> toString n <> " = " <> show e
  show (DPNativeFunction lib name arity _) =
    "native function " <> toString lib <> "." <> toString name <> " " <> show arity
  show (DPMutDeclaration n e) = "mut " <> toString n <> " = " <> show e
  show (DPMutUpdate n e) = show n <> " = " <> show e
  show (DPDeclare n) = "declare " <> toString n

instance Substitutable DesugaredStatement DesugaredExpr where
  substitute s (DSExpr e) = DSExpr $ substitute s e
  substitute s (DSReturn e) = DSReturn $ substitute s e
  substitute (name, expr) (DSDeclaration n e)
    | n == name = DSDeclaration n expr
    | otherwise = DSDeclaration n (substitute (name, expr) e)
  substitute s (DSMutDeclaration n e) = DSMutDeclaration n $ substitute s e
  substitute s (DSMutUpdate n e) = DSMutUpdate n $ substitute s e
  substitute s (DSIf e1 e2 e3) =
    DSIf (substitute s e1) (map (substitute s) e2) (map (substitute s) e3)

instance Substitutable DesugaredExpr DesugaredExpr where
  substitute s (DEVar x)
    | x == fst s = snd s
    | otherwise = DEVar x
  substitute s (DEApplication f args) =
    DEApplication f (map (substitute s) args)
  substitute s (DEList es) = DEList (map (substitute s) es)
  substitute s (DEProperty e i) = DEProperty (substitute s e) i
  substitute s (DEIf e1 e2 e3) =
    DEIf (substitute s e1) (substitute s e2) (substitute s e3)
  substitute s (DETypeOf e) = DETypeOf (substitute s e)
  substitute s (DEIsConstructor e t) = DEIsConstructor (substitute s e) t
  substitute s (DEEqualsTo e1 e2) =
    DEEqualsTo (substitute s e1) (substitute s e2)
  substitute _ (DELiteral l) = DELiteral l
  substitute s (DEDictionary es) = DEDictionary (fmap (substitute s) es)
  substitute s (DEAnd e1 e2) = DEAnd (substitute s e1) (substitute s e2)
  substitute s (DEIndex e1 e2) =
    DEIndex (substitute s e1) (substitute s e2)
  substitute _ DESpecial = DESpecial
  substitute s (DESlice e i) = DESlice (substitute s e) i
  substitute s (DEGreaterThan e1 e2) =
    DEGreaterThan (substitute s e1) e2
  substitute s (DEListLength e) = DEListLength (substitute s e)
  substitute s (DEUnMut e) = DEUnMut (substitute s e)

doesContainReturn :: DesugaredStatement -> Bool
doesContainReturn (DSReturn _) = True
doesContainReturn (DSExpr (DEIf _ e1 e2)) =
  any doesContainReturn e1 || any doesContainReturn e2
doesContainReturn _ = False
