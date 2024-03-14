{-# LANGUAGE DataKinds #-}

module Plume.TypeChecker.Monad.Type.Conversion where

import Plume.Syntax.Common.Type qualified as Pre
import Plume.TypeChecker.Monad
import Plume.TypeChecker.Monad.Type qualified as Post

class To a b where
  convert :: (MonadChecker m) => a -> m b

instance To a a where convert = return

instance Pre.PlumeType `To` Post.PlumeType where
  convert (Pre.TId n) = do
    t <- search @"generics" n
    case t of
      Just t' -> return (Post.TVar t')
      Nothing -> return (Post.TId n)
  convert (Pre.TApp t ts) =
    Post.TApp <$> convert t <*> mapM convert ts
