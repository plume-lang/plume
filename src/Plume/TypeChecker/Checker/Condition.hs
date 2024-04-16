module Plume.TypeChecker.Checker.Condition where

import Plume.Syntax.Abstract qualified as Pre
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.TLIR qualified as Post

synthCond :: Infer -> Infer
synthCond infer (Pre.EConditionBranch cond t f) = local id $ do
  -- Type checking the condition, the true branch and the false branch
  (condTy, cond') <- extractFromArray $ infer cond
  (tTy, t') <- local id . extractFromArray $ infer t
  f' <- maybeM f (local id . extractFromArray . infer)

  -- Unifying the condition type with a boolean type
  condTy `unifiesTo` TBool

  -- Unifying the true branch type with the false branch type
  -- if the false branch is present and return the type of the
  -- condition branch
  f'' <- case f' of
    Nothing -> pure Nothing
    Just (fTy, f'') -> do
      tTy `unifiesTo` fTy
      pure . pure $ f''
  pure (tTy, [Post.EConditionBranch cond' t' f''])
synthCond _ _ = throw $ CompilerError "Only condition branches are supported"