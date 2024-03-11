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
      (Annotation (Maybe t))
      (AbstractExpression t)
      (Maybe (AbstractExpression t))
  | EConditionBranch
      (AbstractExpression t)
      (AbstractExpression t)
      (AbstractExpression t)
  | EClosure
      [Annotation (Maybe t)]
      (Maybe t)
      (AbstractExpression t)
  | EBlock [AbstractExpression t]
  | ERowEmpty
  | ERowExtension Text (AbstractExpression t) (AbstractExpression t)
  | ERowSelect (AbstractExpression t) Text
  | ERowRestrict (AbstractExpression t) Text
  | ELocated (AbstractExpression t) Position
  | ESwitch (AbstractExpression t) [(Pattern, AbstractExpression t)]
  deriving (Eq)

pattern (:>:) :: AbstractExpression t -> Position -> AbstractExpression t
pattern e :>: p = ELocated e p
