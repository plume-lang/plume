module Plume.TypeChecker.Checker.Switch where

import Data.Map qualified as Map
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Literal
import Plume.Syntax.Common.Pattern qualified as Pre
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.TLIR qualified as Post

synthSwitch :: Infer -> Infer
synthSwitch infer (Pre.ESwitch scrutinee cases) = local id $ do
  (t, scrutinee') <- extractFromArray $ infer scrutinee
  (tys, cases') <- mapAndUnzipM (synthCase infer t) cases
  let (ts', expr) = unzip tys

  (ret, xs) <- case ts' of
    [] -> throw EmptyMatch
    (ret : xs) -> return (ret, xs)

  (exprTy, xs') <- case expr of
    [] -> return (ret, [])
    (x : xs'') -> return (x, xs'')

  forM_ xs $ unifiesTo ret
  forM_ xs' $ unifiesTo exprTy

  pure (exprTy, [Post.ESwitch scrutinee' cases'])
synthSwitch _ _ = throw $ CompilerError "Only switches are supported"

synthCase
  :: Infer
  -> PlumeType
  -> (Pre.Pattern, Pre.Expression)
  -> Checker ((PlumeType, PlumeType), (Post.Pattern, Post.Expression))
synthCase infer scrutTy (pat, expr) = local id $ do
  (patTy, patExpr, patEnv) <- synthPattern pat
  (exprTy, expr') <- local id . extractFromArray $ localEnv patEnv (infer expr)
  scrutTy `unifiesTo` patTy
  pure ((patTy, exprTy), (patExpr, expr'))

localEnv :: Map Text PlumeScheme -> Checker a -> Checker a
localEnv env action = do
  vars <- gets (typeEnv . environment)
  insertEnvWith @"typeEnv" (<>) env
  res <- action
  replaceEnv @"typeEnv" vars
  pure res

synthPattern
  :: Pre.Pattern
  -> Checker (PlumeType, Post.Pattern, Map Text PlumeScheme)
synthPattern Pre.PWildcard = do
  t <- fresh
  pure (t, Post.PWildcard, mempty)
synthPattern (Pre.PVariable name) = do
  t <- searchEnv @"datatypeEnv" name
  case t of
    Just t' -> do
      inst <- instantiate t'
      return (inst, Post.PSpecialVar name inst, mempty)
    Nothing -> do
      ty <- fresh
      return
        ( ty
        , Post.PVariable name ty
        , Map.singleton name (Forall [] ty)
        )
synthPattern (Pre.PLiteral l) = do
  let (ty, l') = typeOfLiteral l
  pure (ty, Post.PLiteral l', mempty)
synthPattern (Pre.PConstructor name pats) = do
  t <- searchEnv @"datatypeEnv" name
  case t of
    Just t' -> do
      inst <- instantiate t'
      ret <- fresh
      (patsTy, pats', env) <- mapAndUnzip3M synthPattern pats
      inst `unifiesTo` (patsTy :->: ret)
      return (ret, Post.PConstructor name pats', mconcat env)
    Nothing -> throw $ UnboundVariable name

typeOfLiteral :: Literal -> (PlumeType, Literal)
typeOfLiteral (LInt i) = (TInt, LInt i)
typeOfLiteral (LFloat f) = (TFloat, LFloat f)
typeOfLiteral (LBool b) = (TBool, LBool b)
typeOfLiteral (LString s) = (TString, LString s)
typeOfLiteral (LChar c) = (TChar, LChar c)

mapAndUnzip3M :: (Monad m) => (a -> m (b, c, d)) -> [a] -> m ([b], [c], [d])
mapAndUnzip3M f xs = do
  (bs, cs, ds) <- unzip3 <$> mapM f xs
  pure (bs, cs, ds)