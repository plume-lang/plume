module Plume.TypeChecker.Checker.Declaration where

import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post

synthDecl :: Infer -> Infer
synthDecl
  infer
  (Pre.EDeclaration generics (Annotation name ty) expr body) = do
    convertedGenerics :: [TyVar] <- mapM convert generics
    convertedTy :: PlumeType <- convert ty

    let scheme = Forall convertedGenerics convertedTy
    insertEnv @"typeEnv" name scheme

    (exprTy, expr') <- extractFromArray $ infer expr

    exprTy `unifiesTo` convertedTy

    let newScheme = Forall convertedGenerics exprTy
    insertEnv @"typeEnv" name newScheme

    b <- maybeM body (local id . extractFromArray . infer)
    let retTy = maybe TUnit fst b
    let body' = snd <$> b

    pure (retTy, [Post.EDeclaration (Annotation name exprTy) expr' body'])
synthDecl _ _ = throw $ CompilerError "Only declarations are supported"
