module Plume.TypeChecker.Checker.Closure where

import Data.Map qualified as Map
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post

synthClosure :: Infer -> Infer
synthClosure infer (Pre.EClosure args ret body) = local id $ do
  convertedArgs :: [Annotation PlumeType] <- convert args
  convertedRet :: PlumeType <- convert ret

  let argSchemes = createEnvFromAnnotations convertedArgs
  insertEnvWith @"typeEnv" (<>) argSchemes
  (retTy, body') <- extractFromArray $ infer body

  retTy `unifiesTo` convertedRet

  let closureTy = map (.annotationValue) convertedArgs :->: retTy

  pure (closureTy, [Post.EClosure convertedArgs retTy body'])
synthClosure _ _ = throw $ CompilerError "Only closures are supported"

createEnvFromAnnotations :: [Annotation PlumeType] -> Map Text PlumeScheme
createEnvFromAnnotations xs =
  Map.fromList $ map (\(Annotation n ty) -> (n, Forall [] ty)) xs