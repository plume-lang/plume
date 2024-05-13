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
datatypes :: IORef (Map Text (Map Text PlumeScheme))
datatypes =
  unsafePerformIO $
    newIORef
      ( M.fromList
          [ ("boolean", boolean),
            ("list", list)
          ]
      )

boolean :: Map Text PlumeScheme
boolean = M.fromList [("true", Forall [] $ [] :=>: TBool), ("false", Forall [] $ [] :=>: TBool)]

tA :: PlumeType
tA = TypeQuantified "A"

list :: Map Text PlumeScheme
list =
  M.fromList
    [ ("nil", Forall ["A"] $ [] :=>: TList tA),
      ("cons", Forall ["A"] $ [] :=>: ([tA, TList tA] :->: TList tA))
    ]

synthDataType :: Infer
synthDataType (Pre.EType (Annotation name generics) cons) = do
  convertedGenerics :: [PlumeType] <- mapM convert generics

  let isGeneric (TypeQuantified _) = True
      isGeneric _ = False

      getQVar (TypeQuantified name') = name'
      getQVar _ = error "This should not happen"

  let generics' = if all isGeneric convertedGenerics then map getQVar convertedGenerics else []

  -- Creating the header of the data type: if there are no generics
  -- the header is just the type name, otherwise it becomes a type
  -- constructor.
  let header =
        if null generics
          then TypeId name
          else TypeApp (TypeId name) convertedGenerics

  -- Synthesizing the constructors of the data type
  (cons', m) <- mapAndUnzipM (synthCons generics' ([], header)) cons

  -- Inserting the data type in the metadata
  modifyIORef datatypes (M.insert name (M.unions m))

  return (TUnit, [], pure $ Post.EType name cons')
synthDataType _ = throw $ CompilerError "Only data types are supported"

-- | Used to generate the type constructors of a data type
synthCons ::
  (MonadChecker m) =>
  [QuVar] ->
  ([TyVar], PlumeType) ->
  TypeConstructor Pre.PlumeType ->
  m (TypeConstructor PlumeType, Map Text PlumeScheme)
synthCons qvars (_, header) (TVariable name) = do
  let scheme = Forall qvars $ [] :=>: header
  insertEnv @"datatypeEnv" name scheme

  let dataType = M.singleton name scheme

  pure (TVariable name, dataType)
synthCons qvars (_, header) (TConstructor name args) = do
  args' <- mapM convert args
  let ty = args' :->: header
  let scheme = Forall qvars $ [] :=>: ty
  insertEnv @"datatypeEnv" name scheme

  let dataType = M.singleton name scheme

  pure (TConstructor name args', dataType)
