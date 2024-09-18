module Plume.TypeChecker.Checker.Application where

import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation qualified as Cmm
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Checker.Switch (mapAndUnzip4M)
import Plume.TypeChecker.Constraints.Solver (unifiesWith)
import Plume.TypeChecker.TLIR qualified as Post
import Prelude hiding (local, modify, gets)
import qualified Data.Map as Map
import Plume.TypeChecker.Constraints.Typeclass (createInstName, findM)
import Plume.TypeChecker.Constraints.Unification (compressPaths)

findMatchingDirect :: MonadIO m => PlumeType -> Map PlumeType (Set QuVar, Map Text PlumeScheme) -> m (Maybe (PlumeType, (Set QuVar, Map Text PlumeScheme)))
findMatchingDirect t directExts = do
  t' <- liftIO $ compressPaths t
  let m' = Map.toList directExts
  
  findM m' (\(t'', _) -> t'' `doesUnifyWith'` t')

doesUnifyWith' :: MonadIO m => PlumeType -> PlumeType -> m Bool
doesUnifyWith' t t' = do
  t1 <- liftIO $ compressPaths t
  t2 <- liftIO $ compressPaths t'

  go t1 t2
  where 
    go t1 t2 | t1 == t2 = pure True
    go (TypeId "variable") (TypeId "list") = pure True
    go (TypeId "list") (TypeId "variable") = pure True
    go (TypeVar tv) t1' = do
      tvr <- liftIO $ readIORef tv
      case tvr of
        Link t2' -> go t2' t1'
        _ -> pure False
    go t1' (TypeVar tv) = do
      tvr <- liftIO $ readIORef tv
      case tvr of
        Link t2' -> go t1' t2'
        _ -> pure False
    go (TypeApp t1' ts) (TypeApp t2' ts') = do
      b <- go t1' t2'
      if b
        then and <$> zipWithM go ts ts'
        else pure False
    go (TypeQuantified _) _ = pure True
    go _ (TypeQuantified _) = pure True
    go _ _ = pure False

synthApp :: Infer -> Infer
synthApp infer (Pre.EApplication (Pre.ELocated f _) xs) = synthApp infer (Pre.EApplication f xs)
synthApp infer (Pre.EApplication f@(Pre.EVariable n _) (x:xs)) = do
  (tX, psX, x', _) <- infer x

  tX' <- liftIO $ compressPaths tX

  directExts <- gets (directExtensions . environment)
  (t, ps, f', _) <- do
    res <- findMatchingDirect tX' directExts

    case res of
      Just (ty, (_, schs)) -> case Map.lookup (Cmm.identifier n) schs of
        Just sch -> do
          case tX' of
            TypeVar tv -> do
              tvr <- readIORef tv
              case tvr of
                Link _ -> pure ()
                _ -> throw $ CompilerError "Typechecker couldn't infer the type of the variable"
            _ -> pure ()

          (t, ps) <- instantiate sch

          let finalName = Cmm.identifier n <> "_" <> createInstName ty

          pure (t, ps, pure (Post.EVariable (Cmm.fromText finalName) (Identity t)), False)
        Nothing -> infer (Pre.EVariable n Nothing)
      Nothing -> infer (Pre.EVariable n Nothing) 

  (ts, pss, xs', _) <- mapAndUnzip4M infer xs

  ret <- fresh
  t `unifiesWith` (tX':ts) :->: ret

  let call = Post.EApplication <$> f' <*> sequence (x':xs')
  b <- isAsyncType ret
  
  (modifier, ty) <- if b
    then do
      tvar <- fresh
      TAsync tvar `unifiesWith` ret
      
      pure (await tvar <$> call, tvar)
    else pure (call, ret)

  when (isAsyncCall f || b) $ modify (\s -> s {isAsynchronous = True})

  pure (ty, ps <> psX <> concat pss, modifier, False)
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