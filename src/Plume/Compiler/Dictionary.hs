module Plume.Compiler.Dictionary where

import GHC.IO hiding (liftIO)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Plume.Compiler.ClosureConversion.Syntax qualified as Syn
import Plume.Syntax.Translation.Generics

type ClosureEnv = Map Text Int

type DictReader a = ([Syn.ClosedProgram], a)

type Lambdas = Map Text Syn.ClosedProgram

type MonadDict m = MonadIO m

isLambda :: Text -> Bool
isLambda = Text.isPrefixOf "@lambda"

{-# NOINLINE dictionaryMap #-}
dictionaryMap :: IORef ClosureEnv
dictionaryMap = unsafePerformIO $ newIORef mempty

{-# NOINLINE lambdaMap #-}
lambdaMap :: IORef Lambdas
lambdaMap = unsafePerformIO $ newIORef mempty

{-# NOINLINE currentLambda #-}
currentLambda :: IORef Text
currentLambda = unsafePerformIO $ newIORef ""

fetchLambda :: MonadIO m => Text -> m (Maybe Syn.ClosedProgram)
fetchLambda name = do
  lambdas <- readIORef lambdaMap
  return $ Map.lookup name lambdas

withLocalDict :: (MonadIO m) => ClosureEnv -> m a -> m a
withLocalDict dict m = do
  old <- readIORef dictionaryMap
  modifyIORef' dictionaryMap (Map.union dict)
  res <- m
  writeIORef dictionaryMap old
  return res

transform :: MonadDict m => Syn.ClosedProgram -> m [Syn.ClosedProgram]
transform (Syn.CPFunction name args body) 
  -- Applying transformation only on non-lambda functions
  | not ("@lambda" `Text.isPrefixOf` name) = do
    (ps, body') <- transformS body
    return (ps <> [Syn.CPFunction name args body'])
transform (Syn.CPDeclaration name expr) = do
  (ps, expr') <- transformE expr
  return (ps <> [Syn.CPDeclaration name expr'])
transform (Syn.CPMutDeclaration name expr) = do
  (ps, expr') <- transformE expr
  return (ps <> [Syn.CPMutDeclaration name expr'])
transform (Syn.CPMutUpdate update expr) = do
  (ps1, update') <- transformU update
  (ps2, expr') <- transformE expr
  return (ps1 <> ps2 <> [Syn.CPMutUpdate update' expr'])
transform (Syn.CPStatement st) = do
  (ps, st') <- transformS st
  return (ps <> [Syn.CPStatement st'])
transform p@(Syn.CPNativeFunction {}) = pure [p]
transform p@(Syn.CPFunction name _ _) = do
  modifyIORef' lambdaMap (Map.insert name p)
  return []

transformLambda :: MonadDict m => Syn.ClosedProgram -> m [Syn.ClosedProgram]
transformLambda (Syn.CPFunction name args body) 
  -- Applying transformation only on lambda functions
  | "@lambda" `Text.isPrefixOf` name = do
    writeIORef currentLambda name
    (ps, body') <- transformS body
    return (ps <> [Syn.CPFunction name args body'])
transformLambda _ = pure []

transformE :: MonadDict m => Syn.ClosedExpr -> m (DictReader Syn.ClosedExpr)
transformE (Syn.CEVar x) = return (pure (Syn.CEVar x))
transformE (Syn.CEApplication f args) = do
  f' <- transformE f
  args' <- mapM transformE args
  return (Syn.CEApplication <$> f' <*> sequence args')
transformE (Syn.CELiteral l) = return (pure (Syn.CELiteral l))
transformE (Syn.CEList es) = do
  es' <- mapM transformE es
  return (Syn.CEList <$> sequence es')
transformE (Syn.CEIndex e1 e2) = do
  e1' <- transformE e1
  e2' <- transformE e2
  return (Syn.CEIndex <$> e1' <*> e2')
transformE (Syn.CEAnd e1 e2) = do
  e1' <- transformE e1
  e2' <- transformE e2
  return (Syn.CEAnd <$> e1' <*> e2')
transformE (Syn.CEDeclaration x e1 e2) = do
  e1' <- transformE e1
  e2' <- transformE e2
  return (Syn.CEDeclaration x <$> e1' <*> e2')
transformE (Syn.CEMutDeclaration x e1 e2) = do
  e1' <- transformE e1
  e2' <- transformE e2
  return (Syn.CEMutDeclaration x <$> e1' <*> e2')
transformE (Syn.CEMutUpdate update e1 e2) = do
  update' <- transformU update
  e1' <- transformE e1
  e2' <- transformE e2
  return (Syn.CEMutUpdate <$> update' <*> e1' <*> e2')
transformE (Syn.CEConditionBranch e1 e2 e3) = do
  e1' <- transformE e1
  e2' <- transformE e2
  e3' <- transformE e3
  return (Syn.CEConditionBranch <$> e1' <*> e2' <*> e3')
transformE (Syn.CESwitch e cases) = do
  e' <- transformE e
  cases' <- mapM (\(p, b) -> do
    b' <- transformE b
    return $ (p,) <$> b'
    ) cases
  return (Syn.CESwitch <$> e' <*> sequence cases')
transformE (Syn.CEDictionary es) = do
  case Map.lookup "f" es of
    Just expr@(Syn.CEVar f)
      | isLambda f -> do
        lam <- fetchLambda f
        case (Map.lookup "env" es, lam) of
          (Just (Syn.CEDictionary env), Just prog) -> do
            let envExprs = Map.elems env
            (progs, envExprs') <- mapAndUnzipM transformE envExprs
            let env' = Map.keys env
            let newEnv = Map.fromList $ zip env' [(0 :: Int)..]

            let returnedEnv = Map.fromList $ zip [fromString (show i) | i <- [(0 :: Int)..]] envExprs'
            
            prog' <- withLocalDict newEnv $ do
              transformLambda prog

            let newDict = Map.fromList [("0", Syn.CEDictionary returnedEnv), ("1", expr)]
            pure (concat progs <> prog', Syn.CEDictionary newDict)
          _ -> error "Invalid lambda dictionary"
    _ -> do
      es' <- sequenceMapM transformE (Map.elems es)
      let es'' = zip (Map.keys es) <$> es'
      return (Syn.CEDictionary . Map.fromList <$> es'')
transformE (Syn.CEBlock es) = do
  es' <- mapM transformS es
  return (Syn.CEBlock <$> sequence es')
transformE (Syn.CEProperty e i) = do
  clEnv <- readIORef dictionaryMap
  e' <- transformE e
  case Map.lookup i clEnv of
    Just i' -> return (Syn.CEProperty <$> e' <*> pure (show i'))
    Nothing -> return (Syn.CEProperty <$> e' <*> pure i)

transformE (Syn.CEEqualsType e t) = do
  e' <- transformE e
  return (Syn.CEEqualsType <$> e' <*> pure t)
transformE Syn.CESpecial = return (pure Syn.CESpecial)
transformE (Syn.CEUnMut e) = do
  e' <- transformE e
  return (Syn.CEUnMut <$> e')

transformS :: MonadDict m => Syn.ClosedStatement -> m (DictReader Syn.ClosedStatement)
transformS (Syn.CSExpr e) = do
  e' <- transformE e
  return (Syn.CSExpr <$> e')
transformS (Syn.CSReturn e) = do
  e' <- transformE e
  return (Syn.CSReturn <$> e')
transformS (Syn.CSDeclaration x e) = do
  e' <- transformE e
  return (Syn.CSDeclaration x <$> e')
transformS (Syn.CSConditionBranch e1 e2 e3) = do
  e1' <- transformE e1
  e2' <- transformS e2
  e3' <- transformS e3
  return (Syn.CSConditionBranch <$> e1' <*> e2' <*> e3')
transformS (Syn.CSMutDeclaration x e) = do
  e' <- transformE e
  return (Syn.CSMutDeclaration x <$> e')
transformS (Syn.CSMutUpdate update e) = do
  update' <- transformU update
  e' <- transformE e
  return (Syn.CSMutUpdate <$> update' <*> e')
  
transformU :: MonadDict m => Syn.Update -> m (DictReader Syn.Update)
transformU (Syn.UVariable x) = return (pure (Syn.UVariable x))
transformU (Syn.UProperty update i) = do
  clEnv <- readIORef dictionaryMap
  update' <- transformU update

  case Map.lookup i clEnv of
    Just i' -> return (Syn.UProperty <$> update' <*> pure (show i'))
    Nothing -> return (Syn.UProperty <$> update' <*> pure i)