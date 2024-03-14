module Plume.TypeChecker.Constraints.Solver where

import Plume.Syntax.Concrete (Position)
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Monad

solve :: [TypeConstraint] -> Either (TypeError, Position) Substitution
solve [] = return mempty
solve ((pos, t1 :~: t2) : xs) = do
  let s1 = mgu t1 t2
  case s1 of
    Left err -> Left (err, pos)
    Right s1' -> do
      let s2 = solve $ map (second (apply s1')) xs
      compose s1' <$> s2
solve ((_, Hole _) : xs) = solve xs
