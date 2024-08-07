module Plume.TypeChecker.Checker.Condition where

import Plume.Syntax.Abstract qualified as Pre
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.TLIR qualified as Post
import Plume.TypeChecker.Constraints.Solver (unifiesWith)
import Prelude hiding (local)

synthCond :: Infer -> Infer
synthCond infer (Pre.EConditionBranch cond t f) = local id $ do
  -- Type checking the condition, the true branch and the false branch
  (condTy, ps1, cond', async1) <- infer cond
  (tTy, ps2, t', async2) <- local id $ infer t
  res <- local id $ case f of
    Just f' -> Just <$> infer f'
    Nothing -> pure Nothing
  
  case res of
    Just (fTy, _, _, _) -> do
      tTy `unifiesWith` fTy
    Nothing -> pure ()

  -- Unifying the condition type with a boolean type
  condTy `unifiesWith` TBool

  -- Unifying the true branch type with the false branch type
  -- if the false branch is present and return the type of the
  -- condition branch
  let (ps3, f', async3') = case res of
        Just (_, ps3', f'', async3) -> (ps3', Just <$> f'', Just async3)
        Nothing -> ([], pure Nothing, Nothing)

  pure (tTy, ps1 ++ ps2 ++ ps3 , Post.EConditionBranch <$> cond' <*> t' <*> f', case async3' of
      Just b -> b || async1 || async2
      Nothing -> async1 || async2
    )
synthCond _ _ = throw $ CompilerError "Only condition branches are supported"