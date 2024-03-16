{-# LANGUAGE PatternSynonyms #-}

module Plume.Syntax.Abstract.Expression where

-- Abstract expression is the first intermediate representation of the program
-- that is generated after parsing the concrete syntax and converting it. It
-- removes some concrete syntax details and makes the program more abstract
-- in order to make it easier to manipulate and transform.

import Data.Text hiding (map)
import Plume.Syntax.Common
import Plume.Syntax.Concrete.Expression (Position)
import Prelude hiding (intercalate)

data AbstractExpression t
  = EVariable Text
  | ELiteral Literal
  | EApplication (AbstractExpression t) [AbstractExpression t]
  | EDeclaration
      [PlumeGeneric]
      (Annotation (Maybe t))
      (AbstractExpression t)
      (Maybe (AbstractExpression t))
  | EConditionBranch
      (AbstractExpression t)
      (AbstractExpression t)
      (Maybe (AbstractExpression t))
  | EClosure
      [Annotation (Maybe t)]
      (Maybe t)
      (AbstractExpression t)
  | EBlock [AbstractExpression t]
  | ELocated (AbstractExpression t) Position
  | ESwitch (AbstractExpression t) [(Pattern, AbstractExpression t)]
  | EReturn (AbstractExpression t)
  | ETypeExtension [PlumeGeneric] (Annotation t) [ExtensionMember t]
  | ENativeFunction Text [Text] t
  | EGenericProperty [PlumeGeneric] Text [t] t
  deriving (Eq)

data ExtensionMember t
  = ExtDeclaration
      [PlumeGeneric]
      (Annotation (Maybe t))
      (AbstractExpression t)
  deriving (Eq)

pattern (:>:) :: AbstractExpression t -> Position -> AbstractExpression t
pattern e :>: p = ELocated e p
