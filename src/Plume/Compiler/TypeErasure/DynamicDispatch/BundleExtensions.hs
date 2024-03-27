module Plume.Compiler.TypeErasure.DynamicDispatch.BundleExtensions where

import Data.Text qualified as T
import Plume.Syntax.Common.Annotation
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.TLIR

createName :: PlumeType -> Text
createName (TId n) = n
createName (TApp t ts) = createName t <> ts''
 where
  ts' = map createName ts
  ts'' = if null ts' then "" else "_" <> T.intercalate "_" ts'
createName _ = ""

data Bundled = Bundled
  { bundleName :: Text
  , bundleArgument :: Text
  , bundleType :: PlumeType
  , bundleBody :: TypedExpression PlumeType
  }
  deriving (Eq, Show)

bundleExtensions
  :: Text
  -> [TypedExpression PlumeType]
  -> ([Bundled], [TypedExpression PlumeType], [TypedExpression PlumeType])
bundleExtensions name progs = do
  let progs' = map (bundleExtension name) progs
  let (exts, xs) = unzip progs'
  let (exts', extFuns) = unzip $ catMaybes exts
  let xs' = catMaybes xs
  (exts', xs', extFuns)

bundleExtension
  :: Text
  -> TypedExpression PlumeType
  -> (Maybe (Bundled, TypedExpression PlumeType), Maybe (TypedExpression PlumeType))
bundleExtension name (EExtensionDeclaration n t _ (Annotation arg _) body)
  | n == name = (Just (Bundled n arg t body, fun), Nothing)
 where
  extName = n <> "::" <> createName t
  fun =
    EDeclaration
      (Annotation extName t)
      []
      (EClosure [Annotation arg t] t body)
      Nothing
bundleExtension n (ELocated e _) = bundleExtension n e
bundleExtension _ p = (Nothing, Just p)
