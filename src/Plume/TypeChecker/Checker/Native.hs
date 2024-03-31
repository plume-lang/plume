module Plume.TypeChecker.Checker.Native where

import Data.Map qualified as Map
import Plume.Syntax.Abstract qualified as Pre
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post

synthNative :: Infer
synthNative (Pre.ENativeFunction fp name generics ty) = do
  convertedGenerics <- convert generics
  convertedTy <- convert ty
  let scheme = Forall convertedGenerics convertedTy
  insertEnvWith @"typeEnv" (<>) $ Map.singleton name scheme
  pure
    ( TUnit
    , [Post.ENativeFunction fp name convertedTy]
    )
synthNative _ = throw $ CompilerError "Only native functions are supported"