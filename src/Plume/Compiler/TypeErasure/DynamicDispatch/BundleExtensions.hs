module Plume.Compiler.TypeErasure.DynamicDispatch.BundleExtensions where

import Data.List qualified as List
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
  -> ( [Bundled]
     , [TypedExpression PlumeType]
     , [TypedExpression PlumeType]
     , [TypedExpression PlumeType]
     )
bundleExtensions name progs = do
  let progs' = bundle name progs
  let (before, x, after) = progs'
  let x' = case x of
        Just v | isExtension v name -> Just $ bundleExtension v
        _ -> Nothing

  case x' of
    Just (b, fun) -> do
      let (bundled, bef, aft, funs) = bundleExtensions name after
      (b : bundled, before <> bef, aft, fun : funs)
    Nothing -> ([], before, after, [])

isExtension :: TypedExpression PlumeType -> Text -> Bool
isExtension (EExtensionDeclaration n _ _ _ _) name = n == name
isExtension (ELocated e _) name = isExtension e name
isExtension _ _ = False

findExtensionIndex :: Text -> [TypedExpression PlumeType] -> Maybe Int
findExtensionIndex name progs = do
  List.findIndex (`isExtension` name) progs

bundle
  :: Text
  -> [TypedExpression PlumeType]
  -> ( [TypedExpression PlumeType]
     , Maybe (TypedExpression PlumeType)
     , [TypedExpression PlumeType]
     )
bundle name progs = do
  let idx = findExtensionIndex name progs
  case idx of
    Just i -> do
      let (xs, ys1) = splitAt i progs
      let (y, zs) = case ys1 of
            [] -> (Nothing, [])
            (y' : zs') -> (Just y', zs')
      (xs, y, zs)
    Nothing -> (progs, Nothing, [])

bundleExtension
  :: TypedExpression PlumeType
  -> (Bundled, TypedExpression PlumeType)
bundleExtension (EExtensionDeclaration n t _ (Annotation arg _) body) =
  (Bundled n arg t body, fun)
 where
  extName = n <> "::" <> createName t
  fun =
    EDeclaration
      (Annotation extName t)
      []
      (EClosure [Annotation arg t] t body)
      Nothing
bundleExtension (ELocated e _) = bundleExtension e
bundleExtension _ = error "Invalid extension"
