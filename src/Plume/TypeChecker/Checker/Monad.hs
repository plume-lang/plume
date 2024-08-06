module Plume.TypeChecker.Checker.Monad (
  Infer,
  module Monad,
  isAnnotated,
  isExtAnnotated,
) where

import Plume.Syntax.Abstract qualified as Pre
import Plume.TypeChecker.Monad as Monad
import Plume.TypeChecker.TLIR qualified as Post
import qualified Plume.Syntax.Common as Pre

type Infer = forall m. MonadChecker m => Pre.Expression -> m (PlumeType, [PlumeQualifier], Placeholder Post.Expression, Bool)

isAnnotated :: Pre.Expression -> Bool
isAnnotated (Pre.EClosure args (Just _) _ _) = all (isJust . Pre.annotationValue) args
isAnnotated (Pre.EDeclaration _ ann _ _) = isJust ann.annotationValue
isAnnotated _ = False

isExtAnnotated :: Pre.ExtensionMember -> Bool
isExtAnnotated (Pre.ExtDeclaration _ ann _) = isJust ann.annotationValue
