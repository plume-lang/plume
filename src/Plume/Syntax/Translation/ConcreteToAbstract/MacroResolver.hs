module Plume.Syntax.Translation.ConcreteToAbstract.MacroResolver where

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

convertMacro :: Translator Text CST.Expression AST.Expression
convertMacro f (CST.EMacro name expr) = do
  expr' <- shouldBeAlone <$> f expr
  case expr' of
    Right e -> do
      modifyIORef' macroState $ \st ->
        st {macroVariables = Map.insert name e $ macroVariables st}
      return $ Right Empty
    Left err -> throwError err
convertMacro f (CST.EMacroFunction name args expr) = do
  expr' <- f expr
  case expr' of
    Right e -> do
      modifyIORef' macroState $ \st ->
        st {macroFunctions = Map.insert name (args, e) $ macroFunctions st}
      return $ Right Empty
    Left err -> throwError err
convertMacro _ (CST.EMacroVariable name) = do
  st <- readIORef macroState
  case Map.lookup name $ macroVariables st of
    Just e -> return $ Right $ Single e
    Nothing -> throwError $ "Macro variable " <> name <> " not found"
convertMacro f (CST.EMacroApplication name args) = do
  args' <- fmap flat . sequence <$> mapM f args
  res <- liftIO $ lookupMacro name
  case res of
    Right (argsNames, body) -> do
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
        else throwError "Invalid number of arguments"
    Left err -> throwError err
convertMacro _ _ = error "Impossible happened"

lookupMacro :: Text -> IO (Either Text ([Text], MacroBody))
lookupMacro name = do
  st <- readIORef macroState
  case Map.lookup name $ macroFunctions st of
    Just x -> return $ Right x
    Nothing -> return . Left $ "Macro function " <> name <> " not found"
