{-# LANGUAGE LambdaCase #-}

module Language.Plume.Frontend.TypeChecking.Unification where

import Language.Plume.Syntax.Internal.Type qualified as Ty
import Language.Plume.Frontend.TypeChecking.Monad qualified as M
import Control.Monad.Result qualified as Err
import Data.Map qualified as Map

doesOccur :: IORef Ty.TyVar -> Ty.PlumeType -> IO ()
doesOccur tvr (Ty.MkTyVar tv') = do
  tvr' <- readIORef tvr
  tvr'' <- readIORef tv'
  case tvr'' of
    Ty.Link t -> doesOccur tvr t
    Ty.Unbound name lvl -> do
      let newMinLvl = case tvr' of
            Ty.Link _ -> lvl
            Ty.Unbound _ lvl' -> min lvl' lvl
      writeIORef tv' (Ty.Unbound name newMinLvl)
doesOccur tv (Ty.MkTyApp t1 t2) = do
  doesOccur tv t1
  traverse_ (doesOccur tv) t2
doesOccur _ _ = pure ()

resolveGeneric :: M.MonadChecker m => Ty.PlumeType -> m Ty.PlumeType
resolveGeneric = \case
  Ty.MkTyId i -> do
    gen <- M.gets M.generics
    case Map.lookup i gen of
      Just t -> resolveGeneric t
      Nothing -> pure $ Ty.MkTyId i

  Ty.MkTyApp t1 t2 -> Ty.MkTyApp <$> resolveGeneric t1 <*> traverse resolveGeneric t2

  Ty.MkTyVar tv -> do
    tv' <- liftIO $ readIORef tv
    case tv' of
      Ty.Link t -> resolveGeneric t
      Ty.Unbound _ _ -> pure $ Ty.MkTyVar tv

  Ty.MkTyExists _ _ -> error "Existential types should not be present in the type checker"

-- Unify two types
unify :: M.MonadChecker m => Ty.PlumeType -> Ty.PlumeType -> m ()
unify t t' = do
  t1 <- resolveGeneric t
  t2 <- resolveGeneric t'

  if t1 == t2 then
    pure ()
  else case (t1, t2) of
    (Ty.MkTyVar tv1, _) -> readIORef tv1 >>= \case
      Ty.Link tl -> unify tl t2
      Ty.Unbound _ _ -> liftIO $ do
        doesOccur tv1 t2
        writeIORef tv1 (Ty.Link t2)
    (_, Ty.MkTyVar tv2) -> readIORef tv2 >>= \case
      Ty.Link tl -> unify t1 tl
      Ty.Unbound _ _ -> liftIO $ do
        doesOccur tv2 t1
        writeIORef tv2 (Ty.Link t1)

    (Ty.MkTyApp t1a t1b, Ty.MkTyApp t2a t2b) -> do
      unify t1a t2a
      traverse_ (uncurry unify) $ zip t1b t2b

    (Ty.MkTyId i1, Ty.MkTyId i2) | i1 == i2 -> pure ()

    _ -> M.throw (Err.TypeMismatch t1 t2)

unifyWithMaybe :: M.MonadChecker m => Ty.PlumeType -> Maybe Ty.PlumeType -> m ()
unifyWithMaybe t1 = \case
  Nothing -> pure ()
  Just t2 -> unify t2 t1
