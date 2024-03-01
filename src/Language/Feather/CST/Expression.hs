{-# LANGUAGE PatternSynonyms #-}

module Language.Feather.CST.Expression where

-- Expressions are the most basic form of programs.
-- They permit the user to express more complex programs by combining
-- different expressions together. They can be anything from a simple
-- variable to a complex function application.

import Data.Text hiding (map)
import Language.Feather.CST.Annotation
import Language.Feather.CST.Literal
import Text.Megaparsec.Pos
import Prelude hiding (intercalate)

type Position = (SourcePos, SourcePos)

data BinaryOperator
  = Plus
  | Minus
  | Times
  | Division
  | Mod
  | Equals
  | NotEquals
  | GreaterThan
  | LesserThan
  | StrictlyGreatherThan
  | StrictlyLesserThan
  | And
  | Or

data PrefixOperator
  = Not

data ConcreteExpression t
  = EVariable Text
  | ELiteral Literal
  | EApplication (ConcreteExpression t) [ConcreteExpression t]
  | EBinary BinaryOperator (ConcreteExpression t) (ConcreteExpression t)
  | EPrefix PrefixOperator (ConcreteExpression t)
  | EDeclaration (Annotation (Maybe t)) (ConcreteExpression t) (Maybe (ConcreteExpression t))
  | EConditionBranch (ConcreteExpression t) (ConcreteExpression t) (ConcreteExpression t)
  | EClosure [Annotation (Maybe t)] (Maybe t) (ConcreteExpression t)
  | EBlock [ConcreteExpression t]
  | ERowEmpty
  | ERowExtension Text (ConcreteExpression t) (ConcreteExpression t)
  | ERowSelect (ConcreteExpression t) Text
  | ERowRestrict (ConcreteExpression t) Text
  | ELocated (ConcreteExpression t) Position

pattern (:>:) :: ConcreteExpression t -> Position -> ConcreteExpression t
pattern e :>: p = ELocated e p

instance ToText BinaryOperator where
  toText Plus = "+"
  toText Minus = "-"
  toText Times = "*"
  toText Division = "/"
  toText Mod = "%"
  toText Equals = "=="
  toText NotEquals = "!="
  toText GreaterThan = ">="
  toText LesserThan = "<="
  toText StrictlyGreatherThan = ">"
  toText StrictlyLesserThan = "<"
  toText And = "and"
  toText Or = "or"

instance ToText PrefixOperator where
  toText Not = "not"

instance (ToText a) => ToText (ConcreteExpression a) where
  toText (EVariable n) = n
  toText (ELiteral l) = toText l
  toText (EApplication callee args) = toText callee <> "(" <> intercalate ", " args' <> ")"
    where
      args' = map toText args
  toText (EBinary op lhs rhs) = "(" <> toText lhs <> " " <> toText op <> " " <> toText rhs <> ")"
  toText (EPrefix op expr) = toText op <> " " <> toText expr
  toText (EDeclaration ann value body) = case body of
    Nothing -> toText ann <> " = " <> toText value
    Just body' -> toText ann <> " = " <> toText value <> " in " <> toText body'
  toText (EConditionBranch cond thenBr elseBr) =
    "if "
      <> toText cond
      <> " then "
      <> toText thenBr
      <> " else "
      <> toText elseBr
  toText (EClosure arguments ret body) = case ret of
    Just retType -> strArgs <> ": " <> toText retType <> strBody
    Nothing -> strArgs <> strBody
    where
      arguments' = map toText arguments
      strArgs = "(" <> intercalate ", " arguments' <> ")"
      strBody = " -> " <> toText body
  toText (EBlock exprs) = "{ " <> intercalate "; " (map toText exprs) <> " }"
  toText (ELocated expr _) = toText expr
  toText ERowEmpty = "{}"
  toText r@(ERowExtension {}) = ppExtend (extract r)
    where
      extract :: ConcreteExpression a -> ([(Text, ConcreteExpression a)], ConcreteExpression a)
      extract (ERowExtension label val r') = ((label, val) : names, rest')
        where
          (names, rest') = extract r'
      extract e = ([], e)

      ppExtend :: ([(Text, ConcreteExpression a)], ConcreteExpression a) -> Text
      ppExtend ([], e) = toText e
      ppExtend (names, ERowEmpty) = "{ " <> intercalate ", " (map (\(n, v) -> n <> ": " <> toText v) names) <> " }"
      ppExtend (names, e) = "{ " <> intercalate ", " (map (\(n, v) -> n <> ": " <> toText v) names) <> " | " <> toText e <> " }"
  toText (ERowSelect expr name) = toText expr <> "." <> name
  toText (ERowRestrict expr name) = toText expr <> "\\" <> name
