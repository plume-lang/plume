{-# LANGUAGE LambdaCase #-}

module Plume.TypeChecker.Monad.Conversion where

import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Type qualified as Pre
import Plume.TypeChecker.Monad
import Plume.TypeChecker.Monad.Type qualified as Post

-- | The `ConvertsTo` typeclass is used to convert a type `a` to a type `b`.
-- | It is used to convert the types from the CST to the TLIR.
-- | It could be used for other conversions as well, but it is currently mainly
-- | used to convert types and data-types that contains types.
class a `ConvertsTo` b where
  convert :: (MonadChecker m) => a -> m b

instance Pre.PlumeGeneric `ConvertsTo` [Post.PlumeQualifier] where
  convert (Pre.GVar v) = do
    searchEnv @"datatypeEnv" v >>= \case
      Just _ ->
        throw $ CompilerError "Generic type variable shadowing datatype"
      Nothing -> do
        let ty = Post.TypeQuantified v
        insertEnv @"genericsEnv" v ty
        pure [Post.IsQVar v]
  convert (Pre.GExtends name tcs) = do
    searchEnv @"datatypeEnv" name >>= \case
      Just _ ->
        throw $ CompilerError "Generic type variable shadowing datatype"
      Nothing -> do
        let ty = Post.TypeQuantified name
        insertEnv @"genericsEnv" name ty
        pure (map (Post.IsIn ty) tcs)

instance Pre.PlumeType `ConvertsTo` Post.PlumeType where
  convert (Pre.TId n) = do
    -- Check if the type is bound by a generic type variable
    -- in the generics environment.
    searchEnv @"genericsEnv" n >>= \case
      Just t -> pure t
      Nothing -> pure (Post.TypeId n)
  convert (Pre.TApp x xs) = do
    x' <- convert x
    xs' <- mapM convert xs
    pure $ Post.TypeApp x' xs'

instance (a `ConvertsTo` PlumeType) => Maybe a `ConvertsTo` PlumeType where
  convert = \case
    Just x -> convert x
    Nothing -> fresh

instance Text `ConvertsTo` Post.QuVar where
  convert v = do
    searchEnv @"datatypeEnv" v >>= \case
      Just _ ->
        throw $ CompilerError "Generic type variable shadowing datatype"
      Nothing -> do
        let ty = Post.TypeQuantified v
        insertEnv @"genericsEnv" v ty
        pure v

instance (Maybe Pre.PlumeType, Bool) `ConvertsTo` Post.PlumeType where
  convert (x, True) = TMut <$> convert x
  convert (x, False) = convert x

instance Pre.PlumeGeneric `ConvertsTo` Post.PlumeType where
  convert (Pre.GVar v) = do
    searchEnv @"datatypeEnv" v >>= \case
      Just _ ->
        throw $ CompilerError "Generic type variable shadowing datatype"
      Nothing -> do
        let ty = Post.TypeQuantified v
        insertEnv @"genericsEnv" v ty
        pure ty
  convert _ = throw $ CompilerError "Not implemented"

instance (a `ConvertsTo` b) => [a] `ConvertsTo` [b] where
  convert = mapM convert

instance (a `ConvertsTo` b) => Annotation a `ConvertsTo` Annotation b where
  convert (Annotation n ty) = Annotation n <$> convert ty
