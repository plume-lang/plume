{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Plume.TypeChecker.Monad.Type.Conversion where

import Data.Map qualified as M
import GHC.Records
import Plume.Syntax.Common.Type qualified as Pre
import Plume.Syntax.Concrete.Expression qualified as Pre (TypeConstructor (..))
import Plume.Syntax.Translation.Generics hiding (Error (..))
import Plume.TypeChecker.Monad
import Plume.TypeChecker.Monad.Type qualified as Post
import Prelude hiding (gets)

insert
  :: forall l m a k
   . (MonadChecker m, HasField l CheckerState (M.Map k a), Ord k)
  => k
  -> a
  -> m ()
insert k v = do
  s <- readIORef checkerST
  writeIORef checkerST (setField @l s (M.insert k v (getField @l s)))

class a `To` b where
  convert :: (MonadChecker m) => a -> m b

instance a `To` a where convert = return

instance (a `To` b) => Maybe a `To` Maybe b where
  convert = maybeM convert

instance Pre.TypeConstructor Pre.PlumeType `To` (Text, [Post.PlumeType]) where
  convert (Pre.TVariable n) = return (n, [])
  convert (Pre.TConstructor n ts) = do
    ts' <- mapM convert ts
    return (n, ts')

instance Pre.PlumeType `To` Post.PlumeType where
  convert (Pre.TId n) = do
    t <- search @"generics" n
    case t of
      Just t' -> return (Post.TVar t')
      Nothing -> return (Post.TId n)
  convert (Pre.TApp t ts) =
    Post.TApp <$> convert t <*> mapM convert ts

instance Pre.PlumeGeneric `To` (Post.PlumeGeneric, Map Text Scheme) where
  convert (Pre.GVar n) = do
    ty <- fresh
    insert @"generics" n ty
    return (Post.GVar ty, mempty)
  convert (Pre.GExtends n tys) = do
    ty <- fresh
    insert @"generics" n ty
    tys' <- mapM getScheme tys
    insert @"extendedGenerics" ty tys
    return (Post.GExtends ty tys, M.fromList $ zip tys tys')

instance Pre.PlumeGeneric `To` Post.PlumeGeneric where
  convert (Pre.GVar n) = do
    ty <- fresh
    insert @"generics" n ty
    return (Post.GVar ty)
  convert (Pre.GExtends n tys) = do
    ty <- fresh
    insert @"generics" n ty
    insert @"extendedGenerics" ty tys
    return (Post.GExtends ty tys)

getScheme :: (MonadChecker m) => Text -> m Scheme
getScheme n = do
  s <- gets extensions
  let found =
        findWithKey
          (\(Extension name _ gen _) -> name == n && gen)
          s
  case found of
    Just (_, scheme) -> return scheme
    Nothing -> throw $ CompilerError "Extension not found"

findWithKey
  :: (Ord k) => (k -> Bool) -> M.Map k a -> Maybe (k, a)
findWithKey p m = do
  let m' = M.filterWithKey (\k _ -> p k) m
  case M.toList m' of
    [] -> Nothing
    (x : _) -> Just x