{-# LANGUAGE LambdaCase #-}

module Plume.TypeChecker.Checker.Extension where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Type qualified as Pre
import Plume.TypeChecker.Checker.Closure (createEnvFromAnnotations)
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Constraints.Solver
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post

synthExt :: Infer -> Infer
synthExt
  infer
  (Pre.ETypeExtension generics (Annotation extVar extTy) methods) = do
    methods' <-
      mapM
        ( synthExtMember
            infer
            (extVar, extTy, generics)
        )
        methods

    pure (TUnit, methods')
synthExt _ _ = throw $ CompilerError "Only type extensions are supported"

synthExtMember
  :: Infer
  -> (Text, Pre.PlumeType, [Pre.PlumeGeneric])
  -> Pre.ExtensionMem
  -> Checker Post.Expression
synthExtMember
  infer
  (extVar, preExtTy, extGens)
  ( Pre.ExtDeclaration
      generics
      (Annotation name ty)
      (Pre.EClosure args ret body)
    ) = local id $ do
    convertedGenerics :: [TyVar] <- mapM convert (extGens <> generics)
    convertedTy :: PlumeType <- convert ty
    extTy <- convert preExtTy

    convertedArgs :: [Annotation PlumeType] <- convert args
    convertedRet :: PlumeType <- convert ret

    let preFun = (extTy : map (.annotationValue) convertedArgs) :->: convertedRet
    let sch = Forall convertedGenerics preFun

    let ext = MkExtension name extTy sch
    modifyIORef' checkState $ \s ->
      s {extensions = removeDuplicates $ Set.insert ext s.extensions}

    let argSchemes = createEnvFromAnnotations convertedArgs
    let schemes = Map.insert extVar (Forall [] extTy) argSchemes
    insertEnvWith @"typeEnv" Map.union schemes
    ((retTy, body'), finalExt, s3) <- case convertedGenerics of
      [] -> do
        r@(retTy, _) <- local (\s -> s {returnType = Just convertedRet}) $ extractFromArray (infer body)
        retTy `unifiesTo` convertedRet

        let closureTy = (extTy : map (.annotationValue) convertedArgs) :->: retTy

        convertedTy `unifiesTo` closureTy

        let newScheme = Forall convertedGenerics closureTy
        let newExt = MkExtension name extTy newScheme

        pure (r, newExt, mempty)
      _ -> do
        (r@(retTy, _), cs) <- local (\s -> s {returnType = Just convertedRet}) . getLocalConstraints $ extractFromArray $ infer body
        let closureTy = (extTy : map (.annotationValue) convertedArgs) :->: retTy

        c1 <- createConstraint (convertedTy :~: closureTy)
        c2 <- createConstraint (retTy :~: convertedRet)

        let cs' = cs.tyConstraints <> [c1, c2]

        writeIORef cyclicCounter 0
        s1 <- solve cs'

        let newScheme = Forall (apply s1 convertedGenerics) (apply s1 closureTy)
        let newExt = apply s1 $ MkExtension name extTy newScheme

        modifyIORef' checkState $ \s ->
          s
            { extensions =
                removeDuplicates $ Set.delete ext (extensions s) <> Set.singleton newExt
            }
        (s2, _) <- solveExtend (map (second (apply s1)) cs.extConstraints)
        let s3 = s2 <> s1
        updateSubst s3

        let finalScheme = Forall (apply s3 convertedGenerics) (apply s3 closureTy)
        let finalExt = apply s3 $ MkExtension name extTy finalScheme

        return (r, finalExt, s3)

    modifyIORef' checkState $ \s ->
      s {extensions = removeDuplicates $ Set.insert finalExt (extensions s)}

    mapM_
      ( \case
          Pre.GVar v -> deleteEnv @"genericsEnv" v
          _ -> pure ()
      )
      generics

    pure . apply s3 $
      Post.EExtensionDeclaration
        name
        extTy
        (Annotation extVar extTy)
        (Post.EClosure convertedArgs retTy body')
synthExtMember _ _ _ =
  throw $ CompilerError "Only extension members are supported"

removeDuplicates :: Set Extension -> Set Extension
removeDuplicates xs = Set.fromList $ go (Set.toList xs) []
 where
  go [] acc = acc
  go (x : xs') acc
    | doesExtensionExist x acc = go xs' acc
    | otherwise = go xs' (x : acc)

  doesExtensionExist :: Extension -> [Extension] -> Bool
  doesExtensionExist e@(MkExtension n1 t1 (Forall _ s1)) (MkExtension n2 t2 (Forall _ s2) : xs')
    | n1 == n2 && (isRight (mgu t1 t2) || isRight (mgu s1 s2)) = True
    | otherwise = doesExtensionExist e xs'
  doesExtensionExist _ [] = False