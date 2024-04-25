module Plume.TypeChecker.Checker.Native where

import Data.Map qualified as Map
import Plume.Syntax.Abstract qualified as Pre
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post

synthNative :: Infer
synthNative (Pre.ENativeFunction fp name generics ty st) = do
  nats <- gets natives
  case Map.lookup name nats of
    Just (sch, pos) -> throwRaw (pos, DuplicateNative name sch)
    Nothing -> pure ()

  void (convert generics :: Checker [QuVar])
  convertedTy <- convert ty
  let scheme = convertedTy

  insertEnvWith @"typeEnv" (<>) $ Map.singleton name scheme
  pos <- fetchPosition
  modifyIORef' checkState $ \s ->
    s {natives = Map.insert name (scheme, pos) s.natives}

  pure
    ( TUnit
    , [Post.ENativeFunction fp name convertedTy st]
    )
synthNative _ = throw $ CompilerError "Only native functions are supported"