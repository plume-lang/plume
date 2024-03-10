module Plume.Syntax.Translation.ConcreteToAbstract.MacroResolver where

import Control.Monad.Exception
import Data.Map qualified as Map
import GHC.IO hiding (liftIO)
import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Concrete qualified as CST
import Plume.Syntax.Translation.Generics
import Plume.Syntax.Translation.Substitution

type MacroBody = Spreadable [AST.Expression] AST.Expression

data MacroState = MacroState
  { macroVariables :: Map Text AST.Expression
  , macroFunctions :: Map Text ([Text], MacroBody)
  }

{-# NOINLINE macroState #-}
macroState :: IORef MacroState
macroState = unsafePerformIO $ newIORef $ MacroState mempty mempty

convertMacro :: Translator Error CST.Expression AST.Expression
convertMacro f (CST.EMacro name expr) =
  shouldBeAlone <$> f expr `with` \body -> do
    modifyIORef' macroState $ \st ->
      st {macroVariables = Map.insert name body $ macroVariables st}
    return $ Right Empty
convertMacro f (CST.EMacroFunction name args expr) =
  f expr `with` \body -> do
    modifyIORef' macroState $ \st ->
      st {macroFunctions = Map.insert name (args, body) $ macroFunctions st}
    return $ Right Empty
convertMacro _ (CST.EMacroVariable name) = do
  st <- readIORef macroState
  case Map.lookup name $ macroVariables st of
    Just e -> return $ Right $ Single e
    Nothing -> do
      pos <- readIORef positionRef
      case pos of
        Just p -> throwError $ MacroNotFound name p
        Nothing -> throwError NoPositionSaved
convertMacro f (CST.EMacroApplication name args) = do
  args' <- fmap flat . sequence <$> mapM f args
  liftIO (lookupMacro name) `with` \(argsNames, body) -> do
    if length args == length argsNames
      then do
        let correspondance = zip argsNames <$> args'
        case body of
          Spread es ->
            return $
              (Spread <$>) . substituteManyBlock
                <$> correspondance
                <*> pure es
          Single e ->
            return $
              (Single <$>) . substituteMany
                <$> correspondance
                <*> pure e
          Empty -> return $ Right Empty
      else do
        pos <- readIORef positionRef
        throwError $ case pos of
          Just p -> ArgumentsMismatch argsNames (length args) p
          Nothing -> NoPositionSaved
convertMacro _ _ = error "Impossible happened"

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
