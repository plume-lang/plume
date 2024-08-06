module Plume.TypeChecker.Checker.Application where

import Plume.Syntax.Abstract qualified as Pre
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Checker.Switch (mapAndUnzip4M)
import Plume.TypeChecker.Constraints.Solver (unifiesWith)
import Plume.TypeChecker.TLIR qualified as Post
import Prelude hiding (local, modify)

synthApp :: Infer -> Infer
synthApp infer (Pre.EApplication f xs) = local id $ do
  -- Type checking the function and its arguments
  (t, ps, f'', _) <- infer f

  let orderedArgs = case t of
        args :->: _ -> checkVariableArg args xs
        _ -> xs

  (ts, pss, xs', _) <- mapAndUnzip4M infer orderedArgs

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

  let call = Post.EApplication <$> f'' <*> sequence xs'
  b <- isAsyncType ret
  
  (modifier, ty) <- if b
    then do
      tvar <- fresh
      TAsync tvar `unifiesWith` ret
      
      pure (await tvar <$> call, tvar)
    else pure (call, ret)

  when (isAsyncCall f || b) $ modify (\s -> s {isAsynchronous = True})
    
  pure (ty, ps <> concat pss, modifier, isAsyncCall f || b)
synthApp _ _ = throw $ CompilerError "Only applications are supported"

await :: PlumeType -> Post.Expression -> Post.Expression
await t e = Post.EApplication (Post.EVariable "wait" (Identity ty)) [e]
  where ty = [TAsync t] :->: t

isAsyncCall :: Pre.Expression -> Bool
isAsyncCall (Pre.EVariable "wait" _) = True
isAsyncCall (Pre.ELocated e _) = isAsyncCall e
isAsyncCall _ = False

isAsyncType :: MonadIO m => PlumeType -> m Bool
isAsyncType (TAsync _) = pure True
isAsyncType (TypeVar tv) = do
  tvr <- readIORef tv
  case tvr of
    Unbound _ _ -> pure False
    Link t -> isAsyncType t
isAsyncType _ = pure False

checkVariableArg :: [PlumeType] -> [Pre.Expression] -> [Pre.Expression]
checkVariableArg [] [] = []
checkVariableArg [] _ = []
checkVariableArg _ [] = []
checkVariableArg (t : ts) (x : xs) = case t of
  TVarArg _ -> [Pre.EList (x:xs)]
  _ -> x : checkVariableArg ts xs