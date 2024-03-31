module Plume.TypeChecker.TLIR (
  module TLIR,
  Expression,
  Pattern,
  Program,
) where

import Plume.TypeChecker.TLIR.Syntax as TLIR

import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR.Modules.Pattern as TLIR

type Expression = TypedExpression PlumeType
type Pattern = TypedPattern PlumeType
type Program = [Expression]