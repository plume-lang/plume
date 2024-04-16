module Plume.TypeChecker.Checker.Application where

import Plume.Syntax.Abstract qualified as Pre
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Unification
import Plume.TypeChecker.TLIR qualified as Post

synthApp :: Infer -> Infer
synthApp infer (Pre.EApplication f xs) = local id $ do
  -- Optionally extracting the variable from the located expression in
  -- order to check if it is an extension
  let f' = parseLocated f

  -- Type checking the function and its arguments
  (t, f'') <- extractFromArray $ infer f
  (ts, xs') <- mapAndUnzipM (extractFromArray . infer) xs

  -- Generating a new fresh type variable for the return type and 
  -- unifying the function type given from `f` with the built type from
  -- the arguments and the fresh return type.
  --
  -- For instance, if we have  function `f` with type `fn(t1): int` and
  -- we apply `f` to `x` with type `str`, we should get the following 
  -- constraint: `fn(t1): int ~ fn(str): t2`
  -- When unifying the two types, we get the following substitution:
  -- `t1 ~ str` and `t2 ~ int`
  ret <- fresh 
  t `unifiesTo` ts :->: ret

  -- Checking if the function is an extension
  exts <- gets extensions
  case f' of
    Just (Pre.EVariable name) | doesExtensionExist name exts -> do
      case ts of
        -- If the extension exists, we check if the application type
        -- match at least the requirements of the extension, i.e. the
        -- first type in the list of types which represents the extension
        -- type.
        [] -> throw $ CompilerError "Empty list of types"

        -- Generating a new extension constraint
        (x : _) -> doesExtend x name (ts :->: ret)
    _ -> return ()

  pure (ret, [Post.EApplication f'' xs'])
synthApp _ _ = throw $ CompilerError "Only applications are supported"

-- | Check if an extension exists in the current environment
-- | and return a boolean indicating if it exists or not.
-- | Automatically use the current environment
doesExtensionExistM :: Text -> Checker Bool
doesExtensionExistM name = do
  exts <- gets extensions
  pure . isJust $ find ((== name) . extName) exts

-- | Check extension existence in a given set of extensions
doesExtensionExist :: Text -> Set Extension -> Bool
doesExtensionExist name exts = isJust $ find ((== name) . extName) exts

-- | Check if an expression is an extension
isExtension :: Pre.Expression -> Checker Bool
isExtension (Pre.ELocated expr _) = isExtension expr
isExtension (Pre.EVariable name) = doesExtensionExistM name
isExtension _ = pure False

-- | Extracting variable from a located expression
parseLocated :: Pre.Expression -> Maybe Pre.Expression
parseLocated (Pre.ELocated expr _) = parseLocated expr
parseLocated e@(Pre.EVariable _) = Just e
parseLocated _ = Nothing
