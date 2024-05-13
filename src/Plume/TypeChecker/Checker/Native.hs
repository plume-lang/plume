module Plume.TypeChecker.Checker.Native where

import Data.Map qualified as Map
import Plume.Syntax.Abstract qualified as Pre
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post
import Prelude hiding (gets)

synthNative :: Infer
synthNative (Pre.ENativeFunction fp name generics ty st) = do
  nats <- gets natives
  case Map.lookup name nats of
    Just (sch, pos) -> throwRaw (pos, DuplicateNative name sch)
    Nothing -> pure ()

  gens :: [QuVar] <- convert generics
  convertedTy <- convert ty
  let scheme = Forall gens $ [] :=>: convertedTy

  insertEnvWith @"typeEnv" (<>) $ Map.singleton name scheme
  pos <- fetchPosition
  modifyIORef' checkState $ \s ->
    s {natives = Map.insert name (scheme, pos) s.natives}

  pure
    ( TUnit
    , []
    , pure $ Post.ENativeFunction fp name convertedTy st
    )
synthNative _ = throw $ CompilerError "Only native functions are supported"