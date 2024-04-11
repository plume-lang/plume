{-# LANGUAGE LambdaCase #-}

module Plume.TypeChecker.Monad.Conversion where

import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Type qualified as Pre
import Plume.TypeChecker.Monad
import Plume.TypeChecker.Monad.Type qualified as Post

class a `ConvertsTo` b where
  convert :: a -> Checker b

instance Pre.PlumeType `ConvertsTo` Post.PlumeType where
  convert (Pre.TId n) = do
    searchEnv @"genericsEnv" n >>= \case
      Just t -> pure (Post.TypeVar t)
      Nothing -> pure (Post.TypeId n)
  convert (Pre.TApp x xs) = do
    x' <- convert x
    xs' <- mapM convert xs
    pure $ Post.TypeApp x' xs'

instance (a `ConvertsTo` PlumeType) => Maybe a `ConvertsTo` PlumeType where
  convert = \case
    Just x -> convert x
    Nothing -> fresh

instance Text `ConvertsTo` TyVar where
  convert v = do
    searchEnv @"datatypeEnv" v >>= \case
      Just _ ->
        throw $ CompilerError "Generic type variable shadowing datatype"
      Nothing -> do
        n <- gets nextTyVar
        modify' $ \s -> s {nextTyVar = n + 1}
        let ty = Post.MkTyVar n
        insertEnv @"genericsEnv" v ty
        pure ty

instance (Maybe Pre.PlumeType, Bool) `ConvertsTo` Post.PlumeType where
  convert (x, True) = TMut <$> convert x
  convert (x, False) = convert x

instance Pre.PlumeGeneric `ConvertsTo` Post.TyVar where
  convert (Pre.GVar v) = do
    searchEnv @"datatypeEnv" v >>= \case
      Just _ ->
        throw $ CompilerError "Generic type variable shadowing datatype"
      Nothing -> do
        n <- gets nextTyVar
        modify' $ \s -> s {nextTyVar = n + 1}
        let ty = Post.MkTyVar n
        insertEnv @"genericsEnv" v ty
        pure ty
  convert _ = throw $ CompilerError "Not implemented"

instance Post.TyVar `ConvertsTo` Int where
  convert (MkTyVar n) = pure n

instance (a `ConvertsTo` b) => [a] `ConvertsTo` [b] where
  convert = mapM convert

instance (a `ConvertsTo` b) => Annotation a `ConvertsTo` Annotation b where
  convert (Annotation n ty) = Annotation n <$> convert ty