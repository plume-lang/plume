{-# LANGUAGE PatternSynonyms #-}

module Plume.TypeChecker.TLIR.Synonyms (
  module TLIR,
  pattern (:>:),
  pattern (:<=:),
  pattern (:$:),
) where

import Plume.Syntax.Concrete.Expression (Position)
import Plume.TypeChecker.TLIR.Syntax as TLIR

pattern (:>:) :: TypedExpression t -> Position -> TypedExpression t
pattern e :>: p = ELocated e p

pattern (:<=:) :: Text -> t -> TypedExpression t
pattern n :<=: t = EVariable n t

pattern (:$:) :: TypedExpression t -> [TypedExpression t] -> TypedExpression t
pattern f :$: args = EApplication f args
