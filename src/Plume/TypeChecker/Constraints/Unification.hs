module Plume.TypeChecker.Constraints.Unification where

import Data.Map qualified as M
import Data.Set qualified as S
import Plume.TypeChecker.Monad

variable :: Int -> PlumeType -> Either TypeError Substitution
variable n t
  | t == TVar n = Right M.empty
  | n `S.member` free t =
      Left (InfiniteType n t)
  | otherwise = Right $ M.singleton n t

mgu
  :: PlumeType
  -> PlumeType
  -> Either TypeError Substitution
mgu (TVar i) t = variable i t
mgu t (TVar i) = variable i t
mgu (TApp t1 t2) (TApp t3 t4) = mguMany (t1 : t2) (t3 : t4)
mgu t1@(TId n) t2@(TId n') =
  if n == n'
    then Right M.empty
    else Left (UnificationFail t1 t2)
mgu t1 t2 = Left (UnificationFail t1 t2)

mguMany
  :: [PlumeType]
  -> [PlumeType]
  -> Either TypeError Substitution
mguMany [] [] = Right M.empty
mguMany (t1 : t1s) (t2 : t2s) = do
  let s1 = mgu t1 t2
  case s1 of
    Left err -> Left err
    Right s1' -> do
      let s2 = mguMany (apply s1' t1s) (apply s1' t2s)
      compose s1' <$> s2
mguMany t1s t2s = Left (UnificationMismatch t1s t2s)
