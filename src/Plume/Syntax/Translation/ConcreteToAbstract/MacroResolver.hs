{-# LANGUAGE LambdaCase #-}
module Plume.Syntax.Translation.ConcreteToAbstract.MacroResolver where

import Control.Monad.Exception
import Data.Map qualified as Map
import GHC.IO hiding (liftIO)
import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Concrete qualified as CST
import Plume.Syntax.Translation.Generics
import Plume.Syntax.Translation.Substitution
import Plume.Syntax.Common

-- | Macro body can be a single expression, a spread of expressions
-- | but it can't be empty
type MacroBody = Spreadable [AST.Expression] AST.Expression

-- | Macro state stores all the macros defined in the program
data MacroState = MacroState
  { macroVariables :: Map Text AST.Expression
  , macroFunctions :: Map Text ([Text], MacroBody)
  }

-- | Macro state reference that is used to store the macros
{-# NOINLINE macroState #-}
macroState :: IORef MacroState
macroState = unsafePerformIO $ newIORef $ MacroState mempty mempty

-- | Convert all macro expressions (macro variables and macro applications)
-- | with their corresponding values
convertMacro :: Translator Error CST.Expression AST.Expression
convertMacro f (CST.EMacroFunction name args expr) = do
  let args' = map (\(Annotation n _ _) -> n.identifier) args
  f expr `with` \body -> do
    modifyIORef' macroState $ \st ->
      st {macroFunctions = Map.insert name (args', body) $ macroFunctions st}
    return $ Right Empty
convertMacro f (CST.EMacro name expr) =
  shouldBeAlone <$> f expr `with` \body -> do
    modifyIORef' macroState $ \st ->
      st {macroVariables = Map.insert name body $ macroVariables st}
    return $ Right Empty

-- | Macro variable is just replaced by its macro corresponding
-- | value
convertMacro _ (CST.EMacroVariable name) = do
  st <- readIORef macroState
  case Map.lookup name $ macroVariables st of
    Just e -> return $ Right $ Single e
    Nothing -> do
      pos <- readIORef positionRef
      case pos of
        Just p -> throwError $ MacroNotFound name p
        Nothing -> throwError NoPositionSaved

-- | Macro application is replaced by its corresponding macro body
-- | with the arguments substituted.
convertMacro f (CST.EMacroApplication name args) = do
  args' <- fmap flat . sequence <$> mapM f args
  liftIO (lookupMacro name) `with` \(argsNames, body) -> do
    if length args == length argsNames
      then do
        let correspondance = zip argsNames <$> args'
        return $ substituteSpread <$> correspondance <*> pure body
      else do
        pos <- readIORef positionRef
        throwError $ case pos of
          Just p -> ArgumentsMismatch argsNames (length args) p
          Nothing -> NoPositionSaved
convertMacro _ _ = compilerError "Impossible happened"

-- | Substitute a spreadable expression with a list of expressions
substituteSpread :: [(Text, AST.Expression)] -> MacroBody -> MacroBody
substituteSpread correspondance = \case
  Spread es -> Spread $ substituteManyBlock correspondance es
  Single e -> Single $ substituteMany correspondance e
  Empty -> Empty

-- | Lookup a macro by its name, similar to the `lookup` function
-- | but for macros
lookupMacro :: Text -> IO (Either Error ([Text], MacroBody))
lookupMacro name = do
  st <- readIORef macroState
  case Map.lookup name $ macroFunctions st of
    Just x -> return $ Right x
    Nothing -> do
      pos <- readIORef positionRef
      return $ Left $ case pos of
        Just p -> MacroNotFound name p
        Nothing -> NoPositionSaved
