{-# LANGUAGE OverloadedRecordDot #-}

module Plume.Compiler.Bytecode.Assembler where

import Data.IntMap qualified as IMap
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.IO
import Plume.Compiler.Bytecode.Syntax qualified as BC
import Plume.Compiler.ClosureConversion.Free
import Plume.Compiler.Desugaring.Syntax qualified as Pre
import Plume.Syntax.Common.Literal
import Plume.Syntax.Translation.Generics

instance Free Pre.DesugaredExpr where
  free (Pre.DEVar x) = Set.singleton x
  free (Pre.DEApplication f args) = Set.singleton f <> foldMap free args
  free (Pre.DEList es) = foldMap free es
  free (Pre.DEProperty e _) = free e
  free (Pre.DEIf e1 e2 e3) = free e1 <> free e2 <> free e3
  free (Pre.DETypeOf e) = free e
  free (Pre.DEIsConstructor e _) = free e
  free (Pre.DEEqualsTo e1 e2) = free e1 <> free e2
  free (Pre.DEAnd e1 e2) = free e1 <> free e2
  free (Pre.DEDictionary es) = foldMap free es
  free _ = mempty

instance Free Pre.DesugaredStatement where
  free (Pre.DSExpr e) = free e
  free (Pre.DSReturn e) = free e
  free (Pre.DSDeclaration n e) = Set.singleton n <> free e
  free (Pre.DSConditionBranch e1 e2 e3) = free e1 <> foldMap free e2 <> foldMap free e3

instance Free Pre.DesugaredProgram where
  free (Pre.DPFunction name _ _) = Set.singleton name
  free (Pre.DPStatement s) = free s
  free (Pre.DPNativeFunction _ _) = mempty
  free (Pre.DPDeclaration n _) = Set.singleton n

data AssemblerState = AssemblerState
  { constants :: Map Literal Int
  , nativeFunctions :: Map Text (Int, Int)
  , constructors :: Map Text Int
  , functions :: Set.Set Text
  , metadata :: IntMap BC.FunctionMetaData
  , currentSize :: Int
  , locals :: Map Text Int
  , globals :: Map Text Int
  }
  deriving (Show, Eq)

{-# NOINLINE assemblerState #-}
assemblerState :: IORef AssemblerState
assemblerState =
  unsafePerformIO $
    newIORef $
      AssemblerState mempty mempty mempty mempty mempty 0 mempty mempty

assemble :: Pre.DesugaredExpr -> IO [BC.Instruction]
assemble (Pre.DEVar n) = do
  AssemblerState {functions, locals, globals} <- readIORef assemblerState
  case Map.lookup n globals of
    Just i | n `Set.member` functions -> pure [BC.FLoad i]
    Just i -> pure [BC.LoadGlobal i]
    Nothing -> case Map.lookup n locals of
      Just i -> pure [BC.LoadLocal i]
      Nothing -> error $ "Variable not found: " <> show n
assemble (Pre.DEApplication f args) = do
  AssemblerState {nativeFunctions, functions, locals, globals} <-
    readIORef assemblerState
  args' <- concat <$> mapM assemble (reverse args)
  pure $
    args' ++ case Map.lookup f globals of
      Just i
        | f `Set.member` functions ->
            [BC.FLoad i, BC.Call (length args)]
      Just i -> [BC.LoadGlobal i, BC.Call (length args)]
      Nothing -> case Map.lookup f locals of
        Just i ->
          [BC.LoadLocal i, BC.Call (length args)]
        Nothing -> case Map.lookup f nativeFunctions of
          Just (len, addr) | len == length args -> do
            [BC.NLoad addr, BC.Call (length args)]
          _ -> error $ "Function not found: " <> show f
assemble (Pre.DELiteral l) = do
  AssemblerState {constants} <- readIORef assemblerState
  case Map.lookup l constants of
    Just i' -> pure [BC.CLoad i']
    Nothing -> do
      modifyIORef' assemblerState $ \s ->
        s {constants = Map.insert l (Map.size constants) constants}
      let idx = Map.size constants
      pure [BC.CLoad idx]
assemble (Pre.DEList es) = do
  es' <- concat <$> mapM assemble es
  pure $ es' ++ [BC.MakeList $ length es]
assemble (Pre.DEProperty e i) = do
  e' <- assemble e
  pure $ e' ++ [BC.ListGet i]
assemble (Pre.DEDictionary es) = do
  es' <- concatMapM assemble $ IMap.elems es
  pure $ es' ++ [BC.MakeList $ IMap.size es]
assemble (Pre.DEIf e1 e2 e3) = do
  e1' <- assemble e1
  e2' <- assemble e2
  e3' <- assemble e3
  pure $
    e1'
      ++ [BC.JumpIfRel $ length e2' + 1]
      ++ e2'
      ++ [BC.Jump $ length e3' | not (containsReturn e2')]
      ++ e3'
assemble (Pre.DETypeOf e) = do
  e' <- assemble e
  pure $ e' ++ [BC.TypeOf]
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

assembleStmt :: Pre.DesugaredStatement -> IO [BC.Instruction]
assembleStmt (Pre.DSExpr e) = assemble e
assembleStmt (Pre.DSReturn e) = do
  e' <- assemble e
  pure $ e' ++ [BC.Return]
assembleStmt (Pre.DSConditionBranch e1 e2 e3) = do
  e1' <- assemble e1
  e2' <- concatMapM assembleStmt e2
  e3' <- concatMapM assembleStmt e3
  pure $
    e1'
      ++ [BC.JumpIfRel $ length e2' + 1]
      ++ e2'
      ++ [BC.Jump $ length e3' | not (containsReturn e2')]
      ++ e3'
assembleStmt (Pre.DSDeclaration n e) = do
  e' <- assemble e
  AssemblerState {locals} <- readIORef assemblerState

  case Map.lookup n locals of
    Just i -> do
      modifyIORef' assemblerState $ \s ->
        s {locals = Map.insert n i locals}
      pure $ e' ++ [BC.StoreLocal i]
    Nothing -> error $ "Variable not found: " <> show n

assembleProgram :: Pre.DesugaredProgram -> IO [BC.Instruction]
assembleProgram (Pre.DPFunction n args stmts) = do
  AssemblerState {nativeFunctions, globals} <- readIORef assemblerState
  case Map.lookup n globals of
    Just i -> do
      let freed =
            ( free stmts
                <> Set.fromList args
                <> Set.singleton n
            )
              Set.\\ (Map.keysSet nativeFunctions <> Map.keysSet globals)

      modifyIORef' assemblerState $ \s ->
        s
          { metadata =
              IMap.insert
                i
                ( BC.FunctionMetaData
                    (length args)
                    (s.currentSize + 1)
                    (length freed)
                )
                s.metadata
          , locals = Map.fromList $ zip (Set.toList freed) [0 ..]
          , globals = Map.insert n i globals
          , functions = Set.insert n s.functions
          }
      res <- concatMapM assembleStmt stmts
      return ([BC.MakeLambda (length res)] ++ res ++ [BC.StoreGlobal i])
    Nothing -> error $ "Function not declared: " <> show n
assembleProgram (Pre.DPDeclaration n e) = do
  e' <- assemble e
  AssemblerState {globals} <- readIORef assemblerState
  case Map.lookup n globals of
    Just i -> do
      let res = e' ++ [BC.StoreGlobal i]
      return res
    Nothing -> error $ "Global variable not found: " <> show n
assembleProgram (Pre.DPStatement stmt) = do
  assembleStmt stmt
assembleProgram (Pre.DPNativeFunction n arity) = do
  AssemblerState {nativeFunctions} <- readIORef assemblerState
  case Map.lookup n nativeFunctions of
    Just _ -> error "Native function already declared"
    Nothing -> do
      modifyIORef' assemblerState $ \s ->
        s
          { nativeFunctions = Map.insert n (arity, Map.size nativeFunctions) nativeFunctions
          }
      pure []

getNativeFunctions :: [Pre.DesugaredProgram] -> Set.Set Text
getNativeFunctions = Set.fromList . mapMaybe getNativeFunction
 where
  getNativeFunction (Pre.DPNativeFunction n _) = Just n
  getNativeFunction _ = Nothing

runAssembler :: [Pre.DesugaredProgram] -> IO ([BC.Instruction], AssemblerState)
runAssembler xs = do
  let freed = free xs Set.\\ getNativeFunctions xs
  modifyIORef' assemblerState $ \s ->
    s
      { globals = Map.fromList $ zip (Set.toList freed) [0 ..]
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
  convert (LChar c) = BC.CString (show c)

convertMetadata :: IntMap BC.FunctionMetaData -> [BC.FunctionMetaData]
convertMetadata = IMap.elems

assembleBytecode :: [Pre.DesugaredProgram] -> IO BC.Program
assembleBytecode xs = do
  (is, s) <- runAssembler xs
  let constants = assembleConstants s.constants
  let metadatas = convertMetadata s.metadata
  pure $ BC.Program is constants metadatas