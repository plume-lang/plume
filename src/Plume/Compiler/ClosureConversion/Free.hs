{-# LANGUAGE LambdaCase #-}

module Plume.Compiler.ClosureConversion.Free where

import Data.Set qualified as S
import Plume.Compiler.ClosureConversion.Syntax
import Control.Monad.Exception (compilerError)

class Free a where
  free :: a -> S.Set Text

instance (Free a) => Free [a] where
  free = foldMap free

instance (Free a) => Free (Maybe a) where
  free = foldMap free

instance (Free a, Free b) => Free (a, b) where
  free (a, b) = free a <> free b

instance Free ClosedExpr where
  free (CEVar x) = S.singleton x
  free (CEApplication f args) = free f <> free args
  free (CELiteral _) = S.empty
  free (CEList es) = free es
  free (CEDeclaration x e1 e2) = (free e1 <> free e2) S.\\ S.singleton x
  free (CEConditionBranch e1 e2 e3) = free e1 <> free e2 <> free e3
  free (CESwitch e cases) =
    free e <> foldMap (\(p, b) -> free b S.\\ free p) cases
  free (CEDictionary es) = free es
  free (CEProperty e _) = free e
  free (CEEqualsType e _) = free e
  free (CEBlock ss) = freeBody ss
  free (CEAnd e1 e2) = free e1 <> free e2
  free (CEIndex e1 e2) = free e1 <> free e2
  free CESpecial = S.empty
  free (CEMutDeclaration x e1 e2) = (free e1 <> free e2) S.\\ S.singleton x
  free (CEMutUpdate x e1 e2) = ((free e1 <> free e2) S.\\ free x) <> free x
  free (CEUnMut e) = free e
  free (CEEqualsTo e1 e2) = free e1 <> free e2
  free (CESlice e1 _) = free e1

instance Free Update where
  free (UVariable x) = S.singleton x
  free (UProperty u _) = free u

freeBody :: [ClosedStatement] -> S.Set Text
freeBody body =
  fst $
    foldl'
      ( \(acc, excluded) -> \case
          CSDeclaration n e ->
            (acc `S.union` free e S.\\ S.singleton n, excluded `S.union` S.singleton n)
          CSMutDeclaration n e ->
            (acc `S.union` free e S.\\ S.singleton n, excluded `S.union` S.singleton n)
          x ->
            (acc `S.union` free x S.\\ excluded, excluded)
      )
      (S.empty, S.empty)
      body

instance Free ClosedPattern where
  free (CPVariable x) = S.singleton x
  free (CPLiteral _) = S.empty
  free (CPConstructor _ ps) = free ps
  free CPWildcard = S.empty
  free (CPSpecialVar x) = S.singleton x
  free (CPList ps p) = free ps <> foldMap free p

instance Free ClosedStatement where
  free (CSExpr e) = free e
  free (CSReturn e) = free e
  free (CSDeclaration x e) = free e S.\\ S.singleton x
  free (CSConditionBranch e1 e2 e3) = free e1 <> free e2 <> free e3
  free (CSMutDeclaration x e) = free e S.\\ S.singleton x
  free (CSMutUpdate x e) = (free e S.\\ free x) <> free x
  free (CSWhile e s) = free e <> free s

instance Free ClosedProgram where
  free (CPFunction n args e _) = free e S.\\ (S.fromList args <> S.singleton n)
  free (CPStatement s) = free s
  free (CPNativeFunction {}) = S.empty
  free (CPDeclaration n e) = free e S.\\ S.singleton n
  free (CPMutDeclaration n e) = free e S.\\ S.singleton n
  free (CPMutUpdate n e) = (free e S.\\ free n) <> free n
  free (CPDeclare n) = S.singleton n

instance (Free a) => Free (Map k a) where
  free = foldMap free

instance (Free a) => Free (IntMap a) where
  free = foldMap free

class Substitutable a b where
  substitute :: (Text, b) -> a -> a

instance Substitutable ClosedExpr ClosedExpr where
  substitute (name, expr) (CEVar x)
    | x == name = expr
    | otherwise = CEVar x
  substitute (name, expr) (CEApplication f args) =
    CEApplication (substitute (name, expr) f) (map (substitute (name, expr)) args)
  substitute (name, expr) (CEList es) = CEList (map (substitute (name, expr)) es)
  substitute _ (CELiteral l) = CELiteral l
  substitute (name, expr) (CEDeclaration x e1 e2) =
    CEDeclaration x (substitute (name, expr) e1) (substitute (name, expr) e2)
  substitute (name, expr) (CEConditionBranch e1 e2 e3) =
    CEConditionBranch
      (substitute (name, expr) e1)
      (substitute (name, expr) e2)
      (substitute (name, expr) e3)
  substitute (name, expr) (CESwitch e cases) =
    CESwitch (substitute (name, expr) e) (map proceed cases)
   where
    proceed (p, e') = (p, substitute (name, expr) e')
  substitute (name, expr) (CEDictionary es) = CEDictionary (fmap (substitute (name, expr)) es)
  substitute (name, expr) (CEProperty e i) = CEProperty (substitute (name, expr) e) i
  substitute e (CEEqualsType e' t) = CEEqualsType (substitute e e') t
  substitute e (CEBlock es) = CEBlock (map (substitute e) es)
  substitute e (CEAnd e1 e2) = CEAnd (substitute e e1) (substitute e e2)
  substitute e (CEIndex e1 e2) = CEIndex (substitute e e1) (substitute e e2)
  substitute _ CESpecial = CESpecial
  substitute e (CEMutDeclaration x e1 e2) =
    CEMutDeclaration x (substitute e e1) (substitute e e2)
  substitute e@(n, r) (CEMutUpdate (UVariable x) e1 e2)
    | x == n = CEMutUpdate (convertToUpdate r) (substitute e e1) (substitute e e2)
    | otherwise = CEMutUpdate (UVariable x) (substitute e e1) (substitute e e2)
  substitute e (CEMutUpdate x e1 e2) = CEMutUpdate x (substitute e e1) (substitute e e2)
  substitute e (CEUnMut e') = CEUnMut (substitute e e')
  substitute e (CEEqualsTo e1 e2) = CEEqualsTo (substitute e e1) (substitute e e2)
  substitute e (CESlice e1 e2) = CESlice (substitute e e1) e2

convertToUpdate :: ClosedExpr -> Update
convertToUpdate (CEVar x) = UVariable x
convertToUpdate (CEProperty e f) = UProperty (convertToUpdate e) f
convertToUpdate _ = compilerError "Invalid update"

instance Substitutable ClosedStatement ClosedExpr where
  substitute e (CSExpr e') = CSExpr (substitute e e')
  substitute e (CSReturn e') = CSReturn (substitute e e')
  substitute (name, expr) (CSDeclaration x e) =
    CSDeclaration x (substitute (name, expr) e)
  substitute e (CSConditionBranch e1 e2 e3) =
    CSConditionBranch
      (substitute e e1)
      (substitute e e2)
      (substitute e e3)
  substitute e (CSMutDeclaration x e') = CSMutDeclaration x (substitute e e')
  substitute e@(n, r) (CSMutUpdate (UVariable name) e')
    | name == n = CSMutUpdate (convertToUpdate r) (substitute e e')
    | otherwise = CSMutUpdate (UVariable name) (substitute e e')
  substitute e (CSMutUpdate x e') = CSMutUpdate x (substitute e e')
  substitute e (CSWhile e' s) = CSWhile (substitute e e') (substitute e s)

instance Substitutable ClosedProgram ClosedExpr where
  substitute e (CPStatement s) = CPStatement (substitute e s)
  substitute e (CPFunction name args body isAsync) =
    CPFunction name args (substitute e body) isAsync
  substitute _ p@(CPNativeFunction {}) = p
  substitute e (CPDeclaration name body)
    | name == fst e = CPDeclaration name body
    | otherwise = CPDeclaration name (substitute e body)
  substitute e (CPMutDeclaration name body)
    | name == fst e = CPMutDeclaration name body
    | otherwise = CPMutDeclaration name (substitute e body)
  substitute e@(n, r) (CPMutUpdate (UVariable name) body)
    | name == n = CPMutUpdate (convertToUpdate r) body
    | otherwise = CPMutUpdate (UVariable name) (substitute e body)
  substitute e (CPMutUpdate x body) = CPMutUpdate x (substitute e body)
  substitute _ (CPDeclare name) = CPDeclare name

instance Substitutable ClosedPattern ClosedExpr where
  substitute _ p = p

instance (Substitutable a b) => Substitutable [a] b where
  substitute = fmap . substitute

substituteMany :: (Substitutable a b) => [(Text, b)] -> a -> a
substituteMany = foldr ((.) . substitute) id
