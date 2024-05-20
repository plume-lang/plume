module Plume.TypeChecker.Checker.Closure where

import Data.Map qualified as Map
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Solver (unifiesWith)
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post
import Prelude hiding (local)

synthClosure :: Infer -> Infer
synthClosure infer (Pre.EClosure args ret body) = local id $ do
  convertedArgs :: [Annotation PlumeType] <- convert args
  convertedRet :: PlumeType <- convert ret

  -- Creating a new environment with the arguments as type schemes
  let argSchemes = createEnvFromAnnotations convertedArgs
  insertEnvWith @"typeEnv" (<>) argSchemes

  -- Type checking the body of the closure with the new environment
  -- and the return type of the closure
  (retTy, ps, body') <-
    local (\s -> s {returnType = Just convertedRet}) $ infer body

  -- Unifying specified return type with the inferred return type
  convertedRet `unifiesWith` retTy

  -- Creating the closure type
  let closureTy = map (.annotationValue) convertedArgs :->: retTy
  pos <- fetchPosition

  pure (closureTy, ps, Post.EClosure convertedArgs retTy <$> body' <*> pure pos)
synthClosure _ _ = throw $ CompilerError "Only closures are supported"

-- | Function that create a new environment from a list of converted
-- | arguments.
createEnvFromAnnotations :: [Annotation PlumeType] -> Map Text PlumeScheme
createEnvFromAnnotations xs =
  Map.fromList $ map (\(Annotation n ty) -> (n, Forall [] ([] :=>: ty))) xs
