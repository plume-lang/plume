module Plume.TypeChecker.Checker.Interface where

import Data.Map qualified as Map
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Type qualified as Pre
import Plume.Syntax.Translation.Generics (concatMapM)
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post

synthInterface :: Infer -> Infer
synthInterface _ (Pre.EInterface (Annotation name [ty]) generics methods) = do
  gens' :: [PlumeQualifier] <- concatMapM convert generics
  ty' <- convert ty
  let qvars = getQVars gens'
  let gens = removeQVars gens'

  let inst = IsIn ty' name
  methods' <- mapM convertMethod methods
  let methods'' = fmap (\(n, Forall qv (ps :=>: t)) -> (n, Forall (qv <> qvars) $ (inst : ps) :=>: t)) methods'

  mapM_ (uncurry $ insertEnv @"typeEnv") methods''

  let pred' = gens :=>: inst

  let methodsBis = map (\(n, Forall qv t) -> (n, Forall (qv <> qvars) t)) methods'
  let cls = MkClass qvars pred' (Map.fromList methodsBis)

  addClass name cls

  let mappedMethods = Map.fromList $ sort (map fst methods') `zip` [(0 :: Int) ..]
  modifyIORef' classMapIndex $ Map.union mappedMethods

  let instTy = TypeApp (TypeId name) [ty']
  let genTy = ([instTy] :->:)
  let getIdx n = fromMaybe (-1) (Map.lookup n mappedMethods)
  pos <- fetchPosition
  let genFuns =
        map
          ( \(n, Forall _ (_ :=>: funTy)) ->
              Post.EDeclaration
                (n :@: genTy funTy)
                (Post.EClosure ["$inst" :@: instTy] funTy (Post.EInstanceAccess (Post.EVariable "$inst" instTy) (getIdx n)) pos)
                Nothing
          )
          methods'

  mapM_ (deleteEnv @"genericsEnv" . Pre.getGenericName) generics
  return (TUnit, [], pure $ Post.ESpreadable genFuns)
synthInterface _ _ = throw $ CompilerError "Only interfaces are supported"

convertMethod :: (MonadChecker m) => Annotation Pre.PlumeScheme -> m (Text, PlumeScheme)
convertMethod (Annotation name (Pre.MkScheme gens ty)) = do
  gens' :: [PlumeQualifier] <- concatMapM convert gens

  let qvars = getQVars gens'
  ty' <- convert ty
  pure (name, Forall qvars $ gens' :=>: ty')

buildInterface :: (MonadChecker m) => Text -> [Pre.PlumeType] -> m PlumeType
buildInterface name [] = pure (TypeId name)
buildInterface name tys = do
  tys' <- mapM convert tys
  pure (TypeApp (TypeId name) tys')
