module Plume.TypeChecker.Checker.Extension where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
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
      s {extensions = removeDuplicates $ Set.insert ext s.extensions}

    let argSchemes = createEnvFromAnnotations convertedArgs
    let schemes = Map.insert extVar (Forall [] extTy) argSchemes
    insertEnvWith @"typeEnv" (<>) schemes
    ((retTy, body'), cs) <- getLocalConstraints $ extractFromArray $ infer body

    let closureTy = (extTy : map (.annotationValue) convertedArgs) :->: retTy

    c1 <- createConstraint (convertedTy :~: closureTy)
    c2 <- createConstraint (retTy :~: convertedRet)

    let cs' = cs.tyConstraints <> [c1, c2]

    writeIORef cyclicCounter 0
    s1 <- solve cs'

    let newScheme = Forall (apply s1 $ extGens <> convertedGenerics) (apply s1 closureTy)
    let newExt = apply s1 $ MkExtension name extTy newScheme

    modifyIORef' checkState $ \s ->
      s
        { extensions =
            removeDuplicates $ Set.delete ext (extensions s) <> Set.singleton newExt
        }

    (s2, _) <- resolveCyclic (cs.extConstraints)
    let s3 = s2 <> s1

    modifyIORef' checkState $ \s ->
      s {extensions = removeDuplicates $ Set.insert newExt (extensions s)}

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