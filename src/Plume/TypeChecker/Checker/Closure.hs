module Plume.TypeChecker.Checker.Closure where

import Data.Map qualified as Map
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Solver (unifiesWith)
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post
import Prelude hiding (local, gets, modify)

synthClosure :: Infer -> Infer
synthClosure infer (Pre.EClosure args ret body _) = local id $ do
  convertedArgs :: [Annotation PlumeType] <- convert args
  convertedRet :: PlumeType <- convert ret

  -- Creating a new environment with the arguments as type schemes
  let argSchemes = createEnvFromAnnotations convertedArgs
  insertEnvWith @"typeEnv" (<>) argSchemes

  old <- gets isAsynchronous
  oldRet <- gets returnType
  modify (\s -> s {isAsynchronous = False})

  -- Type checking the body of the closure with the new environment
  -- and the return type of the closure
  (isAsync, (retTy, ps, body')) <-
    local (\s -> s {returnType = Just convertedRet}) $ do
      res <- infer body
      isAsync <- gets isAsynchronous
      return (isAsync, res)

  modify (\s -> s {isAsynchronous = old})

  let retTy' = if isAsync then TypeApp (TypeId "async") [retTy] else retTy

  -- Unifying specified return type with the inferred return type
  convertedRet `unifiesWith` retTy
  
  -- Creating the closure type
  let closureTy = map (.annotationValue) convertedArgs :->: retTy'

  let convertedArgs' = map (fmap Identity) convertedArgs
  let retTy'' = Identity retTy'
  
  modify (\s -> s {returnType = oldRet})

  pure (closureTy, ps, Post.EClosure convertedArgs' retTy'' <$> body' <*> pure isAsync)
synthClosure _ _ = throw $ CompilerError "Only closures are supported"

-- | Function that create a new environment from a list of converted
-- | arguments.
createEnvFromAnnotations :: [Annotation PlumeType] -> Map Text PlumeScheme
createEnvFromAnnotations xs =
  Map.fromList $ map (\(Annotation n ty _) -> (n.identifier, Forall [] ([] :=>: ty))) xs
