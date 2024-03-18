module Plume.Compiler.TypeErasure.DynamicDispatch.BundleExtensions where

import Plume.Compiler.ClosureConversion.Syntax
import Plume.Compiler.TypeErasure.DynamicDispatch.RTTI
import Plume.TypeChecker.Monad.Type

data Bundled = Bundled
  { bundleName :: Text
  , bundleArgument :: Text
  , bundleType :: PlumeType
  , bundleBody :: ClosedStatement
  }
  deriving (Eq, Show, Ord)

bundleExtensions :: Text -> [ClosedProgram] -> ([Bundled], [ClosedProgram])
bundleExtensions name progs = do
  let progs' = map (bundleExtension name) progs
  let (exts, xs) = unzip progs'
  (catMaybes exts, xs)

bundleExtension :: Text -> ClosedProgram -> (Maybe Bundled, ClosedProgram)
bundleExtension name (CPExtFunction t n arg body)
  | n == name = (Just $ Bundled n arg t body, CPFunction extName [arg] body)
 where
  rttiName = rtti t
  extName = n <> "::" <> rttiName
bundleExtension _ p = (Nothing, p)
