module Plume.TypeChecker.Checker.Application where

import Plume.Syntax.Abstract qualified as Pre
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Checker.Switch (mapAndUnzip3M)
import Plume.TypeChecker.Constraints.Solver (unifiesWith)
import Plume.TypeChecker.TLIR qualified as Post
import Prelude hiding (local)

synthApp :: Infer -> Infer
synthApp infer (Pre.EApplication f xs) = local id $ do
  -- Type checking the function and its arguments
  (t, ps, f'') <- infer f
  (ts, pss, xs') <- mapAndUnzip3M infer xs

  -- Generating a new fresh type variable for the return type and
  -- unifying the function type given from `f` with the built type from
  -- the arguments and the fresh return type.
  --
  -- For instance, if we have  function `f` with type `fn(t1): int` and
  -- we apply `f` to `x` with type `str`, we should get the following
  -- constraint: `fn(t1): int ~ fn(str): t2`
  -- When unifying the two types, we get the following substitution:
  -- `t1 ~ str` and `t2 ~ int`
  ret <- fresh
  t `unifiesWith` ts :->: ret

  pure (ret, ps <> concat pss, Post.EApplication <$> f'' <*> sequence xs')
synthApp _ _ = throw $ CompilerError "Only applications are supported"
