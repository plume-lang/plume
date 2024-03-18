module Plume.Compiler.Desugaring.Syntax where

import Plume.Compiler.ClosureConversion.Free
import Plume.Syntax.Common.Literal

data DesugaredExpr
  = DEVar Text
  | DEApplication Text [DesugaredExpr]
  | DELiteral Literal
  | DEList [DesugaredExpr]
  | DEProperty DesugaredExpr Int
  | DEDictionary (IntMap DesugaredExpr)
  | DEIf DesugaredExpr DesugaredExpr DesugaredExpr
  | DETypeOf DesugaredExpr
  | DEIsConstructor DesugaredExpr Text
  | DEEqualsTo DesugaredExpr DesugaredExpr
  | DEAnd DesugaredExpr DesugaredExpr
  deriving (Eq, Show, Ord)

data DesugaredStatement
  = DSExpr DesugaredExpr
  | DSReturn DesugaredExpr
  | DSDeclaration Text DesugaredExpr
  deriving (Eq, Show, Ord)

data DesugaredProgram
  = DPFunction Text [Text] [DesugaredStatement]
  | DPStatement DesugaredStatement
  | DPNativeFunction Text Int
  deriving (Eq, Show, Ord)

instance Substitutable DesugaredStatement DesugaredExpr where
  substitute s (DSExpr e) = DSExpr $ substitute s e
  substitute s (DSReturn e) = DSReturn $ substitute s e
  substitute (name, expr) (DSDeclaration n e)
    | n == name = DSDeclaration n expr
    | otherwise = DSDeclaration n (substitute (name, expr) e)

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