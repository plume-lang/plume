{-# LANGUAGE PatternSynonyms #-}

module Plume.TypeChecker.TLIR.Synonyms (
  module TLIR,
  pattern (:<=:),
  pattern (:$:),
) where

import Plume.TypeChecker.TLIR.Syntax as TLIR

pattern (:<=:) :: Text -> t -> TypedExpression t
pattern n :<=: t = EVariable n t

pattern (:$:) :: TypedExpression t -> [TypedExpression t] -> TypedExpression t
pattern f :$: args = EApplication f args
