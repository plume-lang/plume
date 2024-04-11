{-# LANGUAGE OverloadedRecordDot #-}

module Plume.Compiler.Bytecode.Assembler where

import Data.IntMap qualified as IMap
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text qualified as Text
import GHC.IO
import GHC.Records
import Plume.Compiler.Bytecode.Syntax qualified as BC
import Plume.Compiler.Desugaring.Syntax qualified as Pre
import Plume.Compiler.ClosureConversion.Syntax (Update(..))
import Plume.Syntax.Common.Literal
import Plume.Syntax.Translation.Generics
import System.FilePath

class Free a where
  free :: a -> [Text]

instance (Free a) => Free [a] where
  free = foldMap free

instance Free Pre.DesugaredExpr where
  free (Pre.DEVar x) = [x]
  free (Pre.DEApplication f args) = [f] <> foldMap free args
  free (Pre.DEList es) = foldMap free es
  free (Pre.DEProperty e _) = free e
  free (Pre.DEIf e1 e2 e3) = free e1 <> free e2 <> free e3
  free (Pre.DETypeOf e) = free e
  free (Pre.DEIsConstructor e _) = free e
  free (Pre.DEEqualsTo e1 e2) = free e1 <> free e2
  free (Pre.DEAnd e1 e2) = free e1 <> free e2
  free (Pre.DEDictionary es) = foldMap free es
  free _ = mempty

instance Free Update where
  free (UVariable x) = [x]
  free (UProperty e _) = free e

instance Free Pre.DesugaredStatement where
  free (Pre.DSExpr e) = free e
  free (Pre.DSReturn e) = free e
  free (Pre.DSDeclaration n e) = [n] <> free e
  free (Pre.DSMutDeclaration n e) = [n] <> free e
  free (Pre.DSMutUpdate n e) = free n <> free e

instance Free Pre.DesugaredProgram where
  free (Pre.DPFunction name _ _) = List.singleton name
  free (Pre.DPStatement s) = free s
  free (Pre.DPNativeFunction {}) = mempty
  free (Pre.DPDeclaration n _) = List.singleton n
  free (Pre.DPMutDeclaration n _) = List.singleton n
  free (Pre.DPMutUpdate n _) = free n

data AssemblerState = AssemblerState
  { constants :: Map Literal Int
  , nativeFunctions :: Map Text (Int, Int, Int)
  , metadata :: IntMap BC.FunctionMetaData
  , currentSize :: Int
  , locals :: Map Text Int
  , globals :: Map Text Int
  , nativeLibraries :: [(FilePath, [Text])]
  , cwd :: FilePath
  }
  deriving (Show, Eq)

deriveHasField ''AssemblerState

{-# NOINLINE assemblerState #-}
assemblerState :: IORef AssemblerState
assemblerState =
  unsafePerformIO $
    newIORef $
      AssemblerState mempty mempty mempty 0 mempty mempty mempty mempty

assemble :: Pre.DesugaredExpr -> IO [BC.Instruction]
assemble Pre.DESpecial = pure [BC.Special]
assemble (Pre.DEVar n) = do
  AssemblerState {locals, globals, nativeFunctions} <- readIORef assemblerState
  case Map.lookup n locals of
    Just i -> pure [BC.LoadLocal i]
    Nothing -> case Map.lookup n globals of
      Just i -> pure [BC.LoadGlobal i]
      Nothing -> case Map.lookup n nativeFunctions of
        Just (funLibIdx, name, libAddr) -> pure [BC.LoadNative name libAddr funLibIdx]
        _ -> error $ "Variable not found: " <> show n
assemble (Pre.DEIndex e1 e2) = do
  e1' <- assemble e1
  e2' <- assemble e2
  pure $ e1' ++ e2' ++ [BC.GetIndex]
assemble (Pre.DEApplication f args) = do
  AssemblerState {nativeFunctions, locals, globals} <-
    readIORef assemblerState
  args' <- concat <$> mapM assemble args
  pure $
    args' ++ case Map.lookup f globals of
      Just i -> [BC.LoadGlobal i, BC.Call (length args)]
      Nothing -> case Map.lookup f locals of
        Just i ->
          [BC.LoadLocal i, BC.Call (length args)]
        Nothing -> case Map.lookup f nativeFunctions of
          Just (funLibIdx, name, libAddr) -> do
            [BC.LoadNative name libAddr funLibIdx, BC.Call (length args)]
          _ -> error $ "Function not found: " <> show f
assemble (Pre.DELiteral l) = do
  AssemblerState {constants} <- readIORef assemblerState
  case Map.lookup l constants of
    Just i' -> pure [BC.LoadConstant i']
    Nothing -> do
      modifyIORef' assemblerState $ \s ->
        s {constants = Map.insert l (Map.size constants) constants}
      let idx = Map.size constants
      pure [BC.LoadConstant idx]
assemble (Pre.DEList es) = do
  es' <- concat <$> mapM assemble es
  pure $ es' ++ [BC.MakeList $ length es]
assemble (Pre.DEProperty e i) = do
  e' <- assemble e
  pure $ e' ++ [BC.ListGet i]
assemble (Pre.DEDictionary es) = do
  es' <- concat <$> mapM assemble es
  pure $ es' ++ [BC.MakeList $ length es]
assemble (Pre.DEIf e1 e2 e3) = do
  e1' <- assemble e1
  e2' <- concatMapM assembleStmt e2
  e3' <- concatMapM assembleStmt e3
  pure $
    e1'
      ++ [BC.JumpIfRel $ length e2' + (if containsReturn e2' then 1 else 2)]
      ++ e2'
      ++ [BC.JumpRel $ length e3' + 1 | not (containsReturn e2')]
      ++ e3'
assemble (Pre.DETypeOf e) = do
  e' <- assemble e
  pure $ e' ++ [BC.TypeOf]
assemble (Pre.DEUnMut e) = do
  e' <- assemble e
  pure $ e' ++ [BC.UnMut]
assemble (Pre.DEIsConstructor e t) = do
  e' <- assemble e
  lit <- assemble (Pre.DELiteral $ LString t)
  pure $ e' ++ [BC.ConstructorName] ++ lit ++ [BC.Compare BC.EqualTo]
assemble (Pre.DEEqualsTo e1 e2) = do
  e1' <- assemble e1
  e2' <- assemble e2
  pure $ e1' ++ e2' ++ [BC.Compare BC.EqualTo]
assemble (Pre.DEAnd e1 e2) = do
  e1' <- assemble e1
  e2' <- assemble e2
  pure $ e1' ++ e2' ++ [BC.And]
assemble (Pre.DESlice e i) = do
  e' <- assemble e
  pure $ e' ++ [BC.Slice i]
assemble (Pre.DEGreaterThan e1 e2) = do
  e1' <- assemble e1
  e2' <- assemble (Pre.DELiteral (LInt $ toInteger e2))
  pure $ e1' ++ e2' ++ [BC.Compare BC.GreaterThan]
assemble (Pre.DEListLength e) = do
  e' <- assemble e
  pure $ e' ++ [BC.ListLength]

assembleDecl :: Bool -> Text -> Pre.DesugaredExpr -> IO [BC.Instruction]
assembleDecl isMut n e = do
  e' <- assemble e
  AssemblerState {locals, globals} <- readIORef assemblerState

  let mut = [BC.MakeMutable | isMut]

  case Map.lookup n locals of
    Just i -> do
      modifyIORef' assemblerState $ \s ->
        s {locals = Map.insert n i locals}
      pure $ e' ++ mut ++ [BC.StoreLocal i]
    Nothing -> case Map.lookup n globals of
      Just i -> pure $ e' ++ mut ++ [BC.StoreGlobal i]
      Nothing -> error $ "Variable not found: " <> show n

assembleStmt :: Pre.DesugaredStatement -> IO [BC.Instruction]
assembleStmt (Pre.DSExpr e) = assemble e
assembleStmt (Pre.DSReturn e) = do
  e' <- assemble e
  pure $ e' ++ [BC.Return]
assembleStmt (Pre.DSDeclaration n e) = assembleDecl False n e
assembleStmt (Pre.DSMutDeclaration n e) = assembleDecl True n e
assembleStmt (Pre.DSMutUpdate n e) = do
  e' <- assemble e
  up <- assembleUpdate n
  pure $ e' ++ up ++ [BC.Update]

getUpdateVariable :: Update -> Text
getUpdateVariable (UVariable n) = n
getUpdateVariable (UProperty e _) = getUpdateVariable e

assembleProgram :: Pre.DesugaredProgram -> IO [BC.Instruction]
assembleProgram (Pre.DPFunction n args stmts) = do
  AssemblerState {nativeFunctions, globals} <- readIORef assemblerState
  case Map.lookup n globals of
    Just i -> do
      let freed =
            List.nub (free stmts)
              List.\\ (Map.keys nativeFunctions <> Map.keys globals <> args)

      modifyIORef' assemblerState $ \s ->
        s
          { metadata =
              IMap.insert
                i
                ( BC.FunctionMetaData
                    (length args)
                    (s.currentSize + 1)
                    (length (args <> freed))
                    (Map.fromList $ zip [0 ..] (args <> freed))
                )
                s.metadata
          , locals = Map.fromList $ zip (args <> freed) [0 ..]
          , globals = Map.insert n i globals
          }
      res <- concatMapM assembleStmt stmts

      modifyIORef' assemblerState $ \s ->
        s {locals = mempty, currentSize = s.currentSize + length res}

      return
        ( [BC.MakeLambda (length res) (length (args <> freed))]
            ++ res
            ++ [BC.StoreGlobal i]
        )
    Nothing -> error $ "Function not declared: " <> show n
assembleProgram (Pre.DPDeclaration n e) = do
  e' <- assemble e
  AssemblerState {globals} <- readIORef assemblerState
  case Map.lookup n globals of
    Just i -> do
      let res = e' ++ [BC.StoreGlobal i]
      return res
    Nothing -> error $ "Global variable not found: " <> show n
assembleProgram (Pre.DPMutDeclaration n e) = do
  e' <- assemble e
  AssemblerState {globals} <- readIORef assemblerState
  case Map.lookup n globals of
    Just i -> do
      let res = e' ++ [BC.MakeMutable, BC.StoreGlobal i]
      return res
    Nothing -> error $ "Variable not found: " <> show n
assembleProgram (Pre.DPMutUpdate n e) = do
  e' <- assemble e
  up <- assembleUpdate n
  pure $ e' ++ up ++ [BC.Update]
assembleProgram (Pre.DPStatement stmt) = assembleStmt stmt
assembleProgram (Pre.DPNativeFunction fp n _) = do
  AssemblerState {nativeFunctions, constants, nativeLibraries, cwd} <-
    readIORef assemblerState
  case Map.lookup n nativeFunctions of
    Just _ -> error "Native function already declared"
    Nothing -> do
      i <- case Map.lookup (LString n) constants of
        Just i' -> pure i'
        Nothing -> do
          modifyIORef' assemblerState $ \s ->
            s {constants = Map.insert (LString n) (Map.size constants) constants}
          pure $ Map.size constants
      let path = cwd </> toString fp
      let libIdx = case elemIndexAcc nativeLibraries path 0 of
            Just i' -> i'
            Nothing -> length nativeLibraries

      let lib = List.lookup path nativeLibraries
      funLibIdx <- case lib of
        Just l -> pure $ length l
        Nothing -> pure 0

      modifyIORef' assemblerState $ \s ->
        s
          { nativeFunctions =
              Map.insert
                n
                (funLibIdx, i, libIdx)
                nativeFunctions
          , nativeLibraries = insertWith (<> [n]) nativeLibraries path
          }
      pure []

getNativeFunctions :: [Pre.DesugaredProgram] -> [Text]
getNativeFunctions = mapMaybe getNativeFunction
 where
  getNativeFunction (Pre.DPNativeFunction _ n _) = Just n
  getNativeFunction _ = Nothing

assembleUpdate :: Update -> IO [BC.Instruction]
assembleUpdate (UVariable n) = do
  AssemblerState {locals, globals} <- readIORef assemblerState
  case Map.lookup n locals of
    Just i -> pure [BC.LoadLocal i]
    Nothing -> case Map.lookup n globals of
      Just i -> pure [BC.LoadGlobal i]
      Nothing -> error $ "Variable not found: " <> show n
assembleUpdate (UProperty e p) = do
  e' <- assembleUpdate e
  pure $ e' ++ [BC.ListGet p]

runAssembler :: [Pre.DesugaredProgram] -> IO ([BC.Instruction], AssemblerState)
runAssembler xs = do
  let freed = List.nub (free xs) List.\\ getNativeFunctions xs
  modifyIORef' assemblerState $ \s ->
    s
      { globals = Map.fromList $ zip freed [0 ..]
      }
  is <- concatMapM assembleProgram xs
  s <- readIORef assemblerState
  pure (is, s)

containsReturn :: [BC.Instruction] -> Bool
containsReturn = any isReturn
 where
  isReturn BC.Return = True
  isReturn _ = False

assembleConstants :: Map Literal Int -> [BC.Constant]
assembleConstants = map fst . sortOn snd . map (first convert) . Map.toList
 where
  convert :: Literal -> BC.Constant
  convert (LString s) = BC.CString s
  convert (LInt i) = BC.CInt (fromInteger i)
  convert (LFloat f) = BC.CFloat f
  convert (LBool b) = BC.CInt $ if b then 1 else 0
  convert (LChar c) = BC.CString (Text.pack [c])

convertMetadata :: IntMap BC.FunctionMetaData -> [BC.FunctionMetaData]
convertMetadata = IMap.elems

assembleBytecode :: FilePath -> [Pre.DesugaredProgram] -> IO BC.Program
assembleBytecode fp xs = do
  modifyIORef' assemblerState $ \s -> s {cwd = fp}
  (is, s) <- runAssembler xs
  let constants = assembleConstants s.constants
  let libs = map (second length) s.nativeLibraries
  pure $ BC.Program (optimizeJumps (is ++ [BC.Halt])) constants libs

optimizeJumps :: [BC.Instruction] -> [BC.Instruction]
optimizeJumps (BC.JumpRel 0 : xs) = xs
optimizeJumps [BC.JumpRel _] = []
optimizeJumps (x : xs) = x : optimizeJumps xs
optimizeJumps [] = []

elemIndex :: (Eq a) => [(a, b)] -> a -> Maybe b
elemIndex [] _ = Nothing
elemIndex ((k, v) : xs) k'
  | k == k' = Just v
  | otherwise = elemIndex xs k'

elemIndexAcc :: (Eq a) => [(a, b)] -> a -> Int -> Maybe Int
elemIndexAcc [] _ i = Just i
elemIndexAcc ((k, _) : xs) k' i
  | k == k' = Just i
  | otherwise = elemIndexAcc xs k' (i + 1)

insertWith :: (Eq a, Monoid b) => (b -> b) -> [(a, b)] -> a -> [(a, b)]
insertWith f [] k = [(k, f mempty)]
insertWith f ((k, v) : xs) k'
  | k' == k = (k, f v) : xs
  | otherwise = (k, v) : insertWith f xs k'
