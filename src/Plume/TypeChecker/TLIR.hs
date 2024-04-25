module Plume.TypeChecker.TLIR (
  module TLIR,
  Expression,
  Pattern,
  Program,
  containsReturn,
  isBlock,
) where

import Plume.TypeChecker.TLIR.Syntax as TLIR

import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR.Modules.Pattern as TLIR

type Expression = TypedExpression PlumeType
type Pattern = TypedPattern PlumeType
type Program = [Expression]

containsReturn :: Expression -> Bool
containsReturn (EBlock es) = any containsReturn es
containsReturn (EReturn _) = True
containsReturn (EConditionBranch _ t f) = containsReturn t || maybe False containsReturn f
containsReturn (ESwitch _ cases) = any (containsReturn . snd) cases
containsReturn _ = False

isBlock :: Expression -> Bool
isBlock EBlock{} = True
isBlock _ = False