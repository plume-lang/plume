{-# LANGUAGE LambdaCase #-}

module Plume.Compiler.ClosureConversion.Free where

import Data.Foldable
import Data.Set qualified as S
import Plume.Compiler.ClosureConversion.Syntax

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
  free (CESwitch e cases) = free e <> free cases
  free (CEDictionary es) = free es
  free (CEProperty e _) = free e
  free (CETypeOf e) = free e
  free (CEBlock ss) = freeBody ss

freeBody :: [ClosedStatement] -> S.Set Text
freeBody body =
  fst $
    foldl
      ( \(acc, excluded) -> \case
          CSDeclaration n e ->
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

instance Free ClosedStatement where
  free (CSExpr e) = free e
  free (CSReturn e) = free e
  free (CSDeclaration x e) = free e S.\\ S.singleton x

instance Free ClosedProgram where
  free (CPFunction n args e) = free e S.\\ (S.fromList args <> S.singleton n)
  free (CPStatement s) = free s
  free (CPExtFunction _ n args e) = free e S.\\ (S.singleton args <> S.singleton n)
  free (CPNativeFunction _ _) = S.empty

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
  substitute e (CETypeOf e') = CETypeOf (substitute e e')
  substitute e (CEBlock es) = CEBlock (map (substitute e) es)

instance Substitutable ClosedStatement ClosedExpr where
  substitute e (CSExpr e') = CSExpr (substitute e e')
  substitute e (CSReturn e') = CSReturn (substitute e e')
  substitute (name, expr) (CSDeclaration x e) =
    CSDeclaration x (substitute (name, expr) e)

instance Substitutable ClosedProgram ClosedExpr where
  substitute e (CPStatement s) = CPStatement (substitute e s)
  substitute e (CPFunction name args body) =
    CPFunction name args (substitute e body)
  substitute e (CPExtFunction t name args body) =
    CPExtFunction t name args (substitute e body)
  substitute _ p@(CPNativeFunction _ _) = p

instance Substitutable ClosedPattern ClosedExpr where
  substitute _ p = p

instance (Substitutable a b) => Substitutable [a] b where
  substitute = fmap . substitute

substituteMany :: (Substitutable a b) => [(Text, b)] -> a -> a
substituteMany = foldr ((.) . substitute) id