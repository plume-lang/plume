{-# LANGUAGE PatternSynonyms #-}

module Plume.Syntax.Abstract.Expression where

import Data.Text hiding (map)
import Plume.Syntax.Common
import Plume.Syntax.Concrete.Expression (Position, TypeConstructor)
import Prelude hiding (intercalate)

type IsMutable = Bool

-- | An abstract expression is an expression that is used to represent
-- | a program. It is a more abstract representation of a program than
-- | a concrete syntax tree. It is used to represent the program with
-- | easier constructs to manipulate.
data AbstractExpression t
  = EVariable Text
  | ELiteral Literal
  | EList [AbstractExpression t]
  | EApplication (AbstractExpression t) [AbstractExpression t]
  | EDeclaration
      IsMutable
      [PlumeGeneric]
      (Annotation (Maybe t))
      (AbstractExpression t)
      (Maybe (AbstractExpression t))
  | EType (Annotation [PlumeGeneric]) [TypeConstructor t]
  | EConditionBranch
      (AbstractExpression t)
      (AbstractExpression t)
      (Maybe (AbstractExpression t))
  | EClosure
      [Annotation (Maybe t, IsMutable)]
      (Maybe t)
      (AbstractExpression t)
  | EUnMut (AbstractExpression t)
  | EBlock [AbstractExpression t]
  | ELocated (AbstractExpression t) Position
  | ESwitch (AbstractExpression t) [(Pattern, AbstractExpression t)]
  | EReturn (AbstractExpression t)
  | ETypeExtension [PlumeGeneric] (Annotation t) [ExtensionMember t]
  | ENativeFunction Text Text [Text] t IsStandard
  | EGenericProperty [PlumeGeneric] Text [t] t
  deriving (Show)

type IsStandard = Bool

-- ABSTRACT EXPRESSION INSTANCES

instance (Eq t) => Eq (AbstractExpression t) where
  EVariable n1 == EVariable n2 = n1 == n2
  ELiteral l1 == ELiteral l2 = l1 == l2
  EList es1 == EList es2 = es1 == es2
  EApplication e1 es1 == EApplication e2 es2 = e1 == e2 && es1 == es2
  EDeclaration m1 g1 a1 e1 me1 == EDeclaration m2 g2 a2 e2 me2 =
    m1 == m2
      && g1 == g2
      && a1 == a2
      && e1 == e2
      && me1 == me2
  EType a1 ts1 == EType a2 ts2 = a1 == a2 && ts1 == ts2
  EConditionBranch e11 e12 me1 == EConditionBranch e21 e22 me2 =
    e11 == e21 && e12 == e22 && me1 == me2
  EClosure as1 t1 e1 == EClosure as2 t2 e2 = as1 == as2 && t1 == t2 && e1 == e2
  EBlock es1 == EBlock es2 = es1 == es2
  ELocated e1 _ == ELocated e2 _ = e1 == e2
  ESwitch e1 ps1 == ESwitch e2 ps2 = e1 == e2 && ps1 == ps2
  EReturn e1 == EReturn e2 = e1 == e2
  ETypeExtension gs1 a1 ms1 == ETypeExtension gs2 a2 ms2 =
    gs1 == gs2 && a1 == a2 && ms1 == ms2
  ENativeFunction n1 p1 as1 t1 st1 == ENativeFunction n2 p2 as2 t2 st2 =
    n1 == n2 && p1 == p2 && as1 == as2 && t1 == t2 && st1 == st2
  EGenericProperty gs1 n1 ts1 t1 == EGenericProperty gs2 n2 ts2 t2 =
    gs1 == gs2 && n1 == n2 && ts1 == ts2 && t1 == t2
  _ == _ = False

-- | An extension member is a member that is used to extend a type.
-- | It is currently only a declaration that may be a function.
data ExtensionMember t
  = ExtDeclaration
      [PlumeGeneric]
      (Annotation (Maybe t))
      (AbstractExpression t)
  deriving (Eq, Show)

-- | Shorthand for a located expression
-- | x :>: p is equivalent to ELocated x p
pattern (:>:) :: AbstractExpression t -> Position -> AbstractExpression t
pattern e :>: p = ELocated e p
