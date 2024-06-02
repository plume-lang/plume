{-# LANGUAGE PatternSynonyms #-}
module Plume.Syntax.Concrete (
  module CST, 
  module Pat,
  Expression,
  ExtensionMember,
  Program,
  Pattern,
  pattern EBinary,
  pattern EPrefix,
  pattern EPostfix,
  pattern EUnMut,
  pattern EListIndex,
  pattern EProperty,
  pattern EVarText,

  -- Macro related patterns
  pattern EMacroText,
  pattern EMacroApplication,
  pattern EMacroVariable,
  pattern EMacroFunction,
  pattern EMacro
) where

-- Main core AST module
-- This module re-exports all the other AST modules
-- to be later imported by the parser and the type checker.

import Plume.Syntax.Concrete.Expression as CST hiding (Expression, ExtensionMember)
import Plume.Syntax.Common qualified as Cmm
import Plume.Syntax.Common.Pattern as Pat hiding (Pattern)
import Plume.Syntax.Concrete.Expression qualified as CST

type Expression = CST.Expression Cmm.PlumeType Maybe
type ExtensionMember = CST.ExtensionMember Cmm.PlumeType Maybe
type Pattern = Cmm.Pattern Cmm.PlumeType Maybe
type Program = [Expression]

pattern EVarText :: Text -> Expression
pattern EVarText t = CST.EVariable (Cmm.MkIdentifier t False) Nothing

pattern EBinary :: Text -> Expression -> Expression -> Expression
pattern EBinary op l r = CST.EApplication (EVarText op) [l, r]

pattern EPrefix :: Text -> Expression -> Expression
pattern EPrefix op e = CST.EApplication (EVarText op) [e]

pattern EPostfix :: Text -> Expression -> Expression
pattern EPostfix op e = CST.EApplication (EVarText op) [e]

pattern EUnMut :: Expression -> Expression
pattern EUnMut e = CST.EApplication (CST.EVariable "#deref" Nothing) [e]

pattern EListIndex :: Expression -> Expression -> Expression
pattern EListIndex l i = CST.EApplication (CST.EVariable "get_index" Nothing) [l, i]

pattern EProperty :: Text -> Expression -> Expression
pattern EProperty p e = CST.EApplication (EVarText p) [e]

pattern EMacroText :: Text -> Expression 
pattern EMacroText t = CST.EVariable (Cmm.MkIdentifier t True) Nothing

pattern EMacroApplication :: Text -> [Expression] -> Expression
pattern EMacroApplication n a = CST.EApplication (EMacroText n) a

pattern EMacroVariable :: Text -> Expression
pattern EMacroVariable n = CST.EVariable (Cmm.MkIdentifier n True) Nothing

pattern EMacroFunction :: Text -> [Cmm.Annotation (Maybe Cmm.PlumeType)] -> Expression -> Expression
pattern EMacroFunction n a e = CST.EDeclaration [] (Cmm.Annotation (Cmm.MkIdentifier n True) Nothing False) (CST.EClosure a Nothing e False) Nothing

pattern EMacro :: Text -> Expression -> Expression
pattern EMacro n e = CST.EDeclaration [] (Cmm.Annotation (Cmm.MkIdentifier n True) Nothing False) e Nothing