module Plume.TypeChecker.Constraints.Solver where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Monad

solve :: [PlumeConstraint] -> Checker Substitution
solve [] = pure Map.empty
solve ((p, c) : cs) = do
  stSub <- getSubst
  s <- withPosition p $ solveConstraint (apply stSub c)
  updateSubst s
  s' <- solve (map (second $ apply s) cs)
  pure $ s' <> s

solveConstraint :: TypeConstraint -> Checker Substitution
solveConstraint (t1 :~: t2) = case mgu t1 t2 of
  Right s -> pure s
  Left e -> throw e
solveConstraint (DoesExtend t name funTy@((ty : _) :->: _)) = do
  ext <- findExtensionWithType name funTy t
  s1 <- getSubst
  let MkExtension _ _ sch = ext
  extFun <- instantiate (apply s1 sch)
  case extFun of
    ((ty' : _) :->: _) -> do
      let s2 = mgu ty ty'
      let s3 = mgu funTy extFun
      either throw pure (s3 <> s2)
    _ -> throw $ CompilerError "Only functions are supported in extensions"
solveConstraint (DoesExtend {}) =
  throw $ CompilerError "Only functions are supported in extensions"
solveConstraint (Hole _) =
  throw $ CompilerError "Holes are not supported yet"

findExtensionWithType :: Text -> PlumeType -> PlumeType -> Checker Extension
findExtensionWithType n t fallback = do
  sub <- getSubst
  exts <- apply sub . Set.toList <$> gets extensions
  let found =
        filter
          ( \(MkExtension n' ty (Forall _ sndTy)) ->
              n == n'
                && ( case mgu t sndTy of
                      Right _ -> True
                      Left _ | isNotTVar fallback -> isRight (mgu fallback ty)
                      _ -> False
                   )
          )
          exts
  case found of
    [ext] -> pure ext
    [] -> throw $ NoExtensionFound n t
    _ -> do
      let found' = map (\(MkExtension _ ty _) -> ty) found
      throw $ MultipleExtensionsFound n found' t

isNotTVar :: PlumeType -> Bool
isNotTVar (TypeVar _) = False
isNotTVar _ = True