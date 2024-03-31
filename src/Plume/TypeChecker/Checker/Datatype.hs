module Plume.TypeChecker.Checker.Datatype where

import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Type qualified as Pre
import Plume.Syntax.Concrete.Expression (TypeConstructor (..))
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post

synthDataType :: Infer
synthDataType (Pre.EType (Annotation name generics) cons) = do
  convertedGenerics :: [TyVar] <- mapM convert generics

  let header =
        if null generics
          then TypeId name
          else TypeApp (TypeId name) $ map TypeVar convertedGenerics
  cons' <- mapM (synthCons (convertedGenerics, header)) cons
  return (TUnit, [Post.EType name cons'])
synthDataType _ = throw $ CompilerError "Only data types are supported"

synthCons
  :: ([TyVar], PlumeType)
  -> TypeConstructor Pre.PlumeType
  -> Checker (TypeConstructor PlumeType)
synthCons (gens, header) (TVariable name) = do
  let scheme = Forall gens header
  insertEnv @"datatypeEnv" name scheme

  pure $ TVariable name
synthCons (gens, header) (TConstructor name args) = do
  args' <- mapM convert args
  let ty = args' :->: header
  let scheme = Forall gens ty
  insertEnv @"datatypeEnv" name scheme

  pure $ TConstructor name args'