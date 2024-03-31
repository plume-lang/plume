module Plume.TypeChecker.Checker.Extension where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.TypeChecker.Checker.Closure (createEnvFromAnnotations)
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post

synthExt :: Infer -> Infer
synthExt
  infer
  (Pre.ETypeExtension generics (Annotation extVar extTy) methods) = do
    convertedGenerics :: [TyVar] <- mapM convert generics
    convertedAnnot :: PlumeType <- convert extTy

    methods' <-
      mapM
        ( synthExtMember
            infer
            (extVar, convertedAnnot, convertedGenerics)
        )
        methods

    pure (TUnit, methods')
synthExt _ _ = throw $ CompilerError "Only type extensions are supported"

synthExtMember
  :: Infer
  -> (Text, PlumeType, [TyVar])
  -> Pre.ExtensionMem
  -> Checker Post.Expression
synthExtMember
  infer
  (extVar, extTy, extGens)
  ( Pre.ExtDeclaration
      generics
      (Annotation name ty)
      (Pre.EClosure args ret body)
    ) = local id $ do
    convertedGenerics :: [TyVar] <- mapM convert generics
    convertedTy :: PlumeType <- convert ty

    convertedArgs :: [Annotation PlumeType] <- convert args
    convertedRet :: PlumeType <- convert ret

    let preFun = (extTy : map (.annotationValue) convertedArgs) :->: convertedRet
    let sch = Forall (extGens <> convertedGenerics) preFun

    let ext = MkExtension name extTy sch
    modifyIORef' checkState $ \s ->
      s {extensions = Set.insert ext s.extensions}

    let argSchemes = createEnvFromAnnotations convertedArgs
    let schemes = Map.insert extVar (Forall [] extTy) argSchemes
    insertEnvWith @"typeEnv" (<>) schemes
    (retTy, body') <- extractFromArray $ infer body

    retTy `unifiesTo` convertedRet

    let closureTy = (extTy : map (.annotationValue) convertedArgs) :->: retTy

    convertedTy `unifiesTo` closureTy

    let newScheme = Forall (extGens <> convertedGenerics) closureTy
    let newExt = MkExtension name extTy newScheme

    modifyIORef' checkState $ \s ->
      s {extensions = Set.delete ext (extensions s) <> Set.singleton newExt}

    pure
      ( Post.EExtensionDeclaration
          name
          extTy
          (Annotation extVar extTy)
          (Post.EClosure convertedArgs retTy body')
      )
synthExtMember _ _ _ =
  throw $ CompilerError "Only extension members are supported"
