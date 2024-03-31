module Plume.TypeChecker.Checker.Monad (
  Infer,
  module Monad,
) where

import Plume.Syntax.Abstract qualified as Pre
import Plume.TypeChecker.Monad as Monad
import Plume.TypeChecker.TLIR qualified as Post

type Infer = Pre.Expression -> Checker (PlumeType, [Post.Expression])