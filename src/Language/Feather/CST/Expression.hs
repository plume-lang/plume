{-# LANGUAGE PatternSynonyms #-}

module Language.Feather.CST.Expression where

-- Expressions are the most basic form of programs.
-- They permit the user to express more complex programs by combining
-- different expressions together. They can be anything from a simple
-- variable to a complex function application.

import Data.List
import Language.Feather.CST.Annotation
import Language.Feather.CST.Literal
import Text.Megaparsec.Pos

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
  = EVariable String
  | ELiteral Literal
  | EApplication (ConcreteExpression t) [ConcreteExpression t]
  | EBinary BinaryOperator (ConcreteExpression t) (ConcreteExpression t)
  | EPrefix PrefixOperator (ConcreteExpression t)
  | EDeclaration (Annotation (Maybe t)) (ConcreteExpression t) (Maybe (ConcreteExpression t))
  | EConditionBranch (ConcreteExpression t) (ConcreteExpression t) (ConcreteExpression t)
  | EClosure [Annotation (Maybe t)] (Maybe t) (ConcreteExpression t)
  | EBlock [ConcreteExpression t]
  | ERowEmpty
  | ERowExtension String (ConcreteExpression t) (ConcreteExpression t)
  | ERowSelect (ConcreteExpression t) String
  | ERowRestrict (ConcreteExpression t) String
  | ELocated (ConcreteExpression t) Position

pattern (:>:) :: ConcreteExpression t -> Position -> ConcreteExpression t
pattern e :>: p = ELocated e p

instance Show BinaryOperator where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Division = "/"
  show Mod = "%"
  show Equals = "=="
  show NotEquals = "!="
  show GreaterThan = ">="
  show LesserThan = "<="
  show StrictlyGreatherThan = ">"
  show StrictlyLesserThan = "<"
  show And = "and"
  show Or = "or"

instance Show PrefixOperator where
  show Not = "not"

instance (Show a) => Show (ConcreteExpression a) where
  show (EVariable n) = n
  show (ELiteral l) = show l
  show (EApplication callee args) = show callee ++ "(" ++ intercalate ", " args' ++ ")"
    where
      args' = map show args
  show (EBinary op lhs rhs) = "(" ++ show lhs ++ " " ++ show op ++ " " ++ show rhs ++ ")"
  show (EPrefix op expr) = show op ++ " " ++ show expr
  show (EDeclaration ann value body) = case body of
    Nothing -> show ann ++ " = " ++ show value
    Just body' -> show ann ++ " = " ++ show value ++ " in " ++ show body'
  show (EConditionBranch cond thenBr elseBr) =
    "if "
      ++ show cond
      ++ " then "
      ++ show thenBr
      ++ " else "
      ++ show elseBr
  show (EClosure arguments ret body) = case ret of
    Just retType -> strArgs ++ ": " ++ show retType ++ strBody
    Nothing -> strArgs ++ strBody
    where
      arguments' = map show arguments
      strArgs = "(" ++ intercalate ", " arguments' ++ ")"
      strBody = " -> " ++ show body
  show (EBlock exprs) = "{ " ++ intercalate "; " (map show exprs) ++ " }"
  show (ELocated expr _) = show expr
  show ERowEmpty = "{}"
  show r@(ERowExtension {}) = ppExtend (extract r)
    where
      extract :: ConcreteExpression a -> ([(String, ConcreteExpression a)], ConcreteExpression a)
      extract (ERowExtension label val r') = ((label, val) : names, rest')
        where
          (names, rest') = extract r'
      extract e = ([], e)

      ppExtend :: ([(String, ConcreteExpression a)], ConcreteExpression a) -> String
      ppExtend ([], e) = show e
      ppExtend (names, ERowEmpty) = "{ " ++ intercalate ", " (map (\(n, v) -> n ++ ": " ++ show v) names) ++ " }"
      ppExtend (names, e) = "{ " ++ intercalate ", " (map (\(n, v) -> n ++ ": " ++ show v) names) ++ " | " ++ show e ++ " }"
  show (ERowSelect expr name) = show expr ++ "." ++ name
  show (ERowRestrict expr name) = show expr ++ "\\" ++ name
