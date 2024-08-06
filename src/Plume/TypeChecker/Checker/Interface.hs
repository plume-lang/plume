{-# LANGUAGE LambdaCase #-}
module Plume.TypeChecker.Checker.Interface where

import Data.Map qualified as Map
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Translation.Generics (concatMapM)
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post

synthInterface :: Infer -> Infer
synthInterface _ (Pre.EInterface (Annotation name tys _) generics methods deduction) = do
  gens' :: [PlumeQualifier] <- concatMapM convert generics
  tys' <- mapM convert tys
  let qvars = getQVars gens'
  let gens = removeQVars gens'

  deduction' :: Maybe (QuVar, QuVar) <- case deduction of
    Just (ded, ty) -> Just <$> ((,) <$> convert ded <*> convert ty)
    Nothing -> pure Nothing

  let deduction'' = fmap (bimap TypeQuantified TypeQuantified) deduction'

  let inst = IsIn tys' name.identifier
  methods' <- mapM convertMethod methods
  let methods'' = fmap (\(n, Forall qv (ps :=>: t)) -> (n, Forall (qv <> qvars) $ (inst : ps) :=>: t)) methods'

  mapM_ (uncurry $ insertEnv @"typeEnv") methods''

  let pred' = gens :=>: inst

  let methodsBis = map (\(n, Forall qv t) -> (n, Forall (qv <> qvars) t)) methods'

  let cls = MkClass qvars pred' (Map.fromList methodsBis) deduction''

  addClass name.identifier cls

  let mappedMethods = Map.fromList $ sort (map fst methods') `zip` [(0 :: Int) ..]
  modifyIORef' classMapIndex $ Map.union mappedMethods

  let instTy = TypeApp (TypeId name.identifier) tys'
  let genTy = ([instTy] :->:)
  let getIdx n = fromMaybe (-1) (Map.lookup n mappedMethods)
  let genFuns =
        map
          ( \(n, Forall _ (_ :=>: funTy)) ->
              Post.EDeclaration
                []
                (fromText n :@: Identity (genTy funTy))
                (Post.EClosure ["$inst" :@: Identity instTy] (Identity funTy) (Post.EInstanceAccess (Post.EVariable "$inst" (Identity instTy)) (getIdx n)) False)
                Nothing
          )
          methods'

  mapM_ (deleteEnv @"genericsEnv" . Pre.getGenericName) generics
  return (TUnit, [], pure $ Post.ESpreadable genFuns, False)
synthInterface _ _ = throw $ CompilerError "Only interfaces are supported"

convertMethod :: (MonadChecker m) => Annotation Pre.PlumeScheme -> m (Text, PlumeScheme)
convertMethod (Annotation name (Pre.MkScheme gens ty) _) = do
  gens' :: [PlumeQualifier] <- concatMapM convert gens

  searchEnv @"typeEnv" name.identifier >>= \case
    Just sch -> throw $ FunctionAlreadyExists name.identifier sch
    Nothing -> pure ()

  let qvars = getQVars gens'
  ty' <- convert ty
  pure (name.identifier, Forall qvars $ gens' :=>: ty')

buildInterface :: (MonadChecker m) => Text -> [Pre.PlumeType] -> m PlumeType
buildInterface name [] = pure (TypeId name)
buildInterface name tys = do
  tys' <- mapM convert tys
  pure (TypeApp (TypeId name) tys')
