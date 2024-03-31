module Plume.TypeChecker.Checker.Application where

import Plume.Syntax.Abstract qualified as Pre
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.TLIR qualified as Post

synthApp :: Infer -> Infer
synthApp infer (Pre.EApplication f xs) = local id $ do
  (t, f') <- extractFromArray $ infer f
  (ts, xs') <- mapAndUnzipM (extractFromArray . infer) xs
  ret <- fresh
  t `unifiesTo` ts :->: ret
  pure (ret, [Post.EApplication f' xs'])
synthApp _ _ = throw $ CompilerError "Only applications are supported"

doesExtensionExist :: Text -> Checker Bool
doesExtensionExist name = do
  exts <- gets extensions
  pure . isJust $ find ((== name) . extName) exts

isExtension :: Pre.Expression -> Checker Bool
isExtension (Pre.ELocated expr _) = isExtension expr
isExtension (Pre.EVariable name) = doesExtensionExist name
isExtension _ = pure False

parseLocated :: Pre.Expression -> Checker Pre.Expression
parseLocated (Pre.ELocated expr p) = withPosition p $ parseLocated expr
parseLocated e@(Pre.EVariable _) = pure e
parseLocated _ = throw $ CompilerError "Only variables are supported"