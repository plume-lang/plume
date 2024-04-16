module Plume.TypeChecker.Checker.Datatype where

import Data.Map qualified as M
import GHC.IO
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Type qualified as Pre
import Plume.Syntax.Concrete.Expression (TypeConstructor (..))
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post

-- | Metadata for the data types
{-# NOINLINE datatypes #-}
datatypes :: IORef (Map Text (Map Text Int))
datatypes = unsafePerformIO $ newIORef mempty

synthDataType :: Infer
synthDataType (Pre.EType (Annotation name generics) cons) = do
  convertedGenerics :: [TyVar] <- mapM convert generics

  -- Creating the header of the data type: if there are no generics
  -- the header is just the type name, otherwise it becomes a type
  -- constructor.
  let header =
        if null generics
          then TypeId name
          else TypeApp (TypeId name) $ map TypeVar convertedGenerics

  -- Synthesizing the constructors of the data type
  (cons', m) <- mapAndUnzipM (synthCons (convertedGenerics, header)) cons

  -- Inserting the data type in the metadata
  modifyIORef datatypes (M.insert name (M.unions m))

  return (TUnit, [Post.EType name cons'])
synthDataType _ = throw $ CompilerError "Only data types are supported"

-- | Used to generate the type constructors of a data type
synthCons
  :: ([TyVar], PlumeType)
  -> TypeConstructor Pre.PlumeType
  -> Checker (TypeConstructor PlumeType, Map Text Int)
synthCons (gens, header) (TVariable name) = do
  let scheme = Forall gens header
  insertEnv @"datatypeEnv" name scheme

  let dataType = M.singleton name 0

  pure (TVariable name, dataType)
synthCons (gens, header) (TConstructor name args) = do
  args' <- mapM convert args
  let ty = args' :->: header
  let scheme = Forall gens ty
  insertEnv @"datatypeEnv" name scheme

  let dataType = M.singleton name (length args)

  pure (TConstructor name args', dataType)