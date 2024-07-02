{-# LANGUAGE LambdaCase #-}

module Language.Plume.Frontend.Module.Resolution where

import Language.Plume.Frontend.Module.Monad qualified as M
import Language.Plume.Syntax.HLIR qualified as HLIR
import Control.Monad.Except qualified as Err
import Control.Monad.Position qualified as Pos
import Language.Plume.Frontend.Parser qualified as P
import Language.Plume.Frontend.Parser.Declaration qualified as P
import Data.Map qualified as Map
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Foldable qualified as Fold

import Control.Monad.Result
import System.FilePath

type MonadModule m = (MonadIO m, Err.MonadError (PlumeError, HLIR.Position) m)

getPath :: MonadModule m => FilePath -> Maybe String -> m FilePath
getPath fp ext = do
  m <- readIORef M.moduleState
  let cwd = m.currentDirectory
  let strModName = toString fp -<.> fromMaybe "plm" ext

  pure $ cwd </> strModName

-- | Resolve a filepath to a module unit
-- | This function fetch the true path from a require's path, read the file,
-- | parse it, and return a module unit,
-- |
-- | It may also check if the file is already located in the import stack, and
-- | throwing a cylic import error if it is the case.
resolvePath :: MonadModule m => FilePath -> Bool -> m M.ModuleUnit
resolvePath fp isPublic = do
  m <- readIORef M.moduleState
  newFp <- getPath fp Nothing

  -- Check if the file is already in the import stack
  -- If it is, throw a cyclic import error
  stack <- readIORef M.importStack
  when (newFp `elem` stack) $ do
    pos <- Pos.fetchPosition
    Err.throwError (CyclicModuleDependency newFp, pos)

  case Map.lookup newFp m.resolved of
    Just mu -> pure mu
    Nothing -> do
      -- Add the file to the import stack
      modifyIORef M.importStack (newFp :)

      -- Read the file
      content <- readFileBS newFp
      let contentAsText :: Text = decodeUtf8 content

      -- Parse the file
      parsed <- P.parsePlumeFile newFp contentAsText P.parseProgram

      case parsed of
        Left err -> do
          pos <- Pos.fetchPosition
          Err.throwError (ParseError err, pos)
        Right ast -> do
          -- Create the module unit
          let mu = mempty
                { M.name = newFp
                , M.path = newFp
                , M.public = isPublic
                , M.imports = []
                , M.variables = Fold.foldMap getDefinedVariables ast
                }

          -- Add the module unit to the module state
          modifyIORef M.moduleState $ \s -> s
            { M.resolved = Map.insert newFp mu (M.resolved s) }

          modifyIORef M.resultState (ast <>)

          -- Checking for undefined variables and types
          void $ foldMapM (`getModuleUnit` mu) ast

          -- Getting all module units from the AST requires
          mu' <- mapM resolveRequires ast

          -- Remove the file from the import stack
          modifyIORef M.importStack (List.delete newFp)

          pure (mu { M.imports = mu' })

getDefinedVariables :: HLIR.AST "declaration" -> Set Text
getDefinedVariables (HLIR.MkDeclPublic (HLIR.MkDeclLocated _ decl))
  = getDefinedVariables (HLIR.MkDeclPublic decl)
getDefinedVariables (HLIR.MkDeclLocated _ decl)
  = getDefinedVariables decl
getDefinedVariables (HLIR.MkDeclPublic (HLIR.MkDeclFunction name _ _ _ _))
  = Set.singleton name
getDefinedVariables (HLIR.MkDeclPublic (HLIR.MkDeclVariable name _ _))
  = Set.singleton name.name
getDefinedVariables _ = mempty

resolveRequires
  :: MonadModule m
  => HLIR.AST "declaration"
  -> m M.ModuleUnit
resolveRequires (HLIR.MkDeclRequire path)
  = resolvePath (toString path) False
resolveRequires (HLIR.MkDeclPublic (HLIR.MkDeclRequire path))
  = resolvePath (toString path) True
resolveRequires (HLIR.MkDeclLocated pos e) = do
  Pos.pushPosition pos
  mu <- resolveRequires e
  void Pos.popPosition
  pure mu
resolveRequires _ = pure mempty

getGeneric :: HLIR.PlumeType -> Text
getGeneric (HLIR.MkTyId name) = name
getGeneric _ = ""

getModuleUnit
  :: MonadModule m
  => HLIR.AST "declaration"
  -> M.ModuleUnit
  -> m M.ModuleUnit
getModuleUnit (HLIR.MkDeclFunction name generics args ret body) m = do
  let args' = map (.name) args
  let generics' = map getGeneric generics
  let mu = m
        { M.variables = Set.insert name (M.variables m)
        , M.types = Set.fromList generics' <> M.types m
        }

  mapM_ (traverse (`checkForTypeF` mu)) args
  checkForTypeF ret mu

  let tmpMu = mu { M.variables = M.variables m <> Set.fromList args' <> Set.singleton name }
  checkForUndefined body tmpMu

  pure mu
getModuleUnit (HLIR.MkDeclVariable name _ expr) m = do
  let mu = m
        { M.variables = Set.insert name.name (M.variables m)
        }

  checkForTypeF name.value mu
  checkForUndefined expr mu

  pure mu
getModuleUnit (HLIR.MkDeclPublic (HLIR.MkDeclLocated pos decl)) m = do
  Pos.pushPosition pos
  mu <- getModuleUnit (HLIR.MkDeclPublic decl) m
  void Pos.popPosition
  pure mu
getModuleUnit (HLIR.MkDeclLocated pos decl) m = do
  Pos.pushPosition pos
  mu <- getModuleUnit decl m
  void Pos.popPosition
  pure mu
getModuleUnit (HLIR.MkDeclRequire path) m =
  (m <>) <$> resolvePath (toString path) False
getModuleUnit (HLIR.MkDeclNative name generics args ret) m = do
  let generics' = map getGeneric generics
  let mu = m
        { M.variables = Set.insert name (M.variables m)
        , M.types = Set.fromList generics' <> M.types m
        }

  mapM_ (`checkForType` mu) args
  checkForType ret mu

  pure mu
getModuleUnit _ m = pure m

checkForUndefined
  :: MonadModule m
  => HLIR.AST "expression"
  -> M.ModuleUnit
  -> m ()
checkForUndefined (HLIR.MkExprVariable (HLIR.MkAnnotation name _)) m = do
  unless (Set.member name (M.variables m)) $ do
    pos <- Pos.fetchPosition
    Err.throwError (VariableNotFound name, pos)
checkForUndefined (HLIR.MkExprCall f args _) m =
  checkForUndefined f m *> mapM_ (`checkForUndefined` m) args
checkForUndefined (HLIR.MkExprIf cond t f) m =
  checkForUndefined cond m *> checkForUndefined t m *> checkForUndefined f m
checkForUndefined (HLIR.MkExprLiteral _) _ = pure ()
checkForUndefined (HLIR.MkExprBlock exprs _) m = mapM_ (`checkForUndefined` m) exprs
checkForUndefined (HLIR.MkExprLambda args ret body) m = do
  let args' = map (.name) args
  let mu = m
        { M.variables = Set.fromList args' <> m.variables
        }

  checkForTypeF ret mu
  mapM_ (traverse (`checkForTypeF` mu)) args
  checkForUndefined body mu
checkForUndefined (HLIR.MkExprAnnotation e ann) m = do
  checkForType ann m
  checkForUndefined e m
checkForUndefined (HLIR.MkExprLocated _ e) m = checkForUndefined e m
checkForUndefined (HLIR.MkExprReturn e) m = checkForUndefined e m
checkForUndefined (HLIR.MkExprSwitch e cases _) m = do
  checkForUndefined e m
  mapM_ (\(p, e') -> checkForUndefined e' m *> checkForPattern p m) cases

checkForPattern :: MonadModule m => HLIR.AST "pattern" -> M.ModuleUnit -> m ()
checkForPattern (HLIR.MkPatternVariable _) _ = pure ()
checkForPattern (HLIR.MkPatternLocated _ p) m = checkForPattern p m
checkForPattern _ _ = pure ()

checkForTypeF
  :: (Monad f, MonadModule m, Traversable f)
  => f HLIR.PlumeType
  -> M.ModuleUnit
  -> m ()
checkForTypeF t m =  mapM_ (`checkForType` m) t

checkForType
  :: MonadModule m
  => HLIR.PlumeType
  -> M.ModuleUnit
  -> m ()
checkForType (HLIR.MkTyApp f tys) m = checkForType f m *> mapM_ (`checkForType` m) tys
checkForType (HLIR.MkTyId name) m = do
  unless (Set.member name (M.types m)) $ do
    pos <- Pos.fetchPosition
    Err.throwError (TypeNotFound name, pos)
checkForType (HLIR.MkTyVar _) _ = pure ()
checkForType _ _ = pure ()

removeRequires :: [HLIR.AST "declaration"] -> [HLIR.AST "declaration"]
removeRequires = filter (not . isRequire)
  where
    isRequire (HLIR.MkDeclRequire _) = True
    isRequire (HLIR.MkDeclPublic x) = isRequire x
    isRequire (HLIR.MkDeclLocated _ x) = isRequire x
    isRequire _ = False

removePublics :: HLIR.AST "declaration" -> HLIR.AST "declaration"
removePublics (HLIR.MkDeclPublic x) = removePublics x
removePublics (HLIR.MkDeclLocated pos x) = HLIR.MkDeclLocated pos (removePublics x)
removePublics x = x

resolveModules
  :: MonadIO m
  => [HLIR.AST "declaration"]
  -> M.ModuleState
  -> m (Either (PlumeError, HLIR.Position) [HLIR.AST "declaration"])
resolveModules exprs ms = do
  writeIORef M.moduleState ms
  writeIORef M.resultState exprs

  Err.runExceptT (Fold.foldlM (flip getModuleUnit) mempty exprs) >>= \case
    Left err -> return $ Left err
    Right _ -> do
      res <- readIORef M.resultState <&> Right
      writeIORef M.resultState []

      pure $ map removePublics . removeRequires <$> res

createModuleState
  :: MonadIO m
  => FilePath
  -> m M.ModuleState
createModuleState fp = do
  let cwd = takeDirectory fp

  let ms = M.MkModuleState {
    M.initialPath = fp,
    M.currentDirectory = cwd,
    M.resolved = Map.empty,
    M.standardPath = Nothing,
    M.boundArgs = [],
    M.modulePath = Nothing
  }

  pure ms
