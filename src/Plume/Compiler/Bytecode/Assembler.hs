{-# LANGUAGE OverloadedRecordDot #-}

module Plume.Compiler.Bytecode.Assembler where

import Data.IntMap qualified as IMap
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text qualified as Text
import Plume.Compiler.Bytecode.Syntax qualified as BC
import Plume.Compiler.Desugaring.Syntax qualified as Pre
import Plume.Compiler.ClosureConversion.Syntax (Update(..))
import Plume.Syntax.Common.Literal
import Plume.Syntax.Translation.Generics
import Plume.Compiler.Bytecode.Arithmetic (compileFunction)
import Control.Monad.Exception (compilerError)

assembleCondition :: Pre.DesugaredExpr -> IO ([BC.Instruction], Int -> BC.Instruction)
assembleCondition (Pre.DEEqualsTo e1 (Pre.DELiteral l@(LInt _))) = do
  e1' <- assemble e1
  l' <- BC.assembleLit l
  pure (e1', \i -> BC.IJumpElseRelCmpConstant i BC.EqualTo l')
assembleCondition (Pre.DEEqualsTo e1 (Pre.DELiteral l)) = do
  e1' <- assemble e1
  l' <- BC.assembleLit l
  pure (e1', \i -> BC.JumpElseRelCmpConstant i BC.EqualTo l')
assembleCondition (Pre.DEEqualsTo e1 e2) = do
  e1' <- assemble e1
  e2' <- assemble e2
  pure (e1' ++ e2', (`BC.JumpElseRelCmp` BC.EqualTo))
assembleCondition (Pre.DEGreaterThan e1 e2) = do
  e1' <- assemble e1
  e2' <- BC.assembleLit (LInt $ toInteger e2)
  pure (e1', \i -> BC.IJumpElseRelCmpConstant i BC.GreaterThan e2')
assembleCondition (Pre.DEAnd e1 e2) = do
  e1' <- assemble e1
  e2' <- assemble e2
  pure (e1' ++ e2', (`BC.JumpElseRelCmp` BC.AndCmp))
assembleCondition (Pre.DEApplication "or::bool" [e1, e2]) = do
  e1' <- assemble e1
  e2' <- assemble e2
  pure (e1' ++ e2', (`BC.JumpElseRelCmp` BC.OrCmp))
assembleCondition (Pre.DEApplication "<=::int" [e1, e2]) = do
  e1' <- assemble e1
  e2' <- assemble e2
  pure (e1' ++ e2', (`BC.IJumpElseRelCmp` BC.LessThanOrEqualTo))
assembleCondition (Pre.DEApplication ">=::int" [e1, e2]) = do
  e1' <- assemble e1
  e2' <- assemble e2
  pure (e1' ++ e2', (`BC.IJumpElseRelCmp` BC.GreaterThanOrEqualTo))
assembleCondition (Pre.DEApplication "!=::int" [e1, e2]) = do
  e1' <- assemble e1
  e2' <- assemble e2
  pure (e1' ++ e2', (`BC.IJumpElseRelCmp` BC.NotEqualTo))
assembleCondition (Pre.DEApplication "==::int" [e1, e2]) = do
  e1' <- assemble e1
  e2' <- assemble e2
  pure (e1' ++ e2', (`BC.IJumpElseRelCmp` BC.EqualTo))
assembleCondition e = do
  e' <- assemble e
  pure (e', BC.JumpElseRel)

assemble :: Pre.DesugaredExpr -> IO [BC.Instruction]
assemble Pre.DESpecial = pure [BC.Special]
assemble (Pre.DEVar n) = do
  BC.AssemblerState {BC.locals, BC.globals, BC.nativeFunctions} <- readIORef BC.assemblerState
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
assemble (Pre.DEApplication "<=::int" [x, y]) = do
  x' <- assemble x
  y' <- assemble y
  pure $ x' ++ y' ++ [BC.Compare BC.LessThanOrEqualTo]
assemble app@(Pre.DEApplication {}) = compileFunction assemble app
assemble (Pre.DELiteral l) = do
  l' <- BC.assembleLit l
  pure [BC.LoadConstant l']
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
  (e1', f) <- assembleCondition e1
  e2' <- concatMapM assembleStmt e2
  e3' <- concatMapM assembleStmt e3
  pure $
    e1'
      ++ [f $ length e2' + (if containsReturn e2' then 1 else 2)]
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
  BC.AssemblerState {BC.locals, BC.globals} <- readIORef BC.assemblerState

  let mut = [BC.MakeMutable | isMut]

  case Map.lookup n locals of
    Just i -> do
      modifyIORef' BC.assemblerState $ \s ->
        s {BC.locals = Map.insert n i locals}
      pure $ e' ++ mut ++ [BC.StoreLocal i]
    Nothing -> case Map.lookup n globals of
      Just i -> pure $ e' ++ mut ++ [BC.StoreGlobal i]
      Nothing -> error $ "Variable not found: " <> show n

assembleStmt :: Pre.DesugaredStatement -> IO [BC.Instruction]
assembleStmt (Pre.DSExpr e) = assemble e
assembleStmt (Pre.DSReturn (Pre.DELiteral l)) = do
  e' <- BC.assembleLit l
  pure [BC.ReturnConst e']
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
  BC.AssemblerState {BC.nativeFunctions, BC.globals} <- readIORef BC.assemblerState
  case Map.lookup n globals of
    Just i -> do
      let freed =
            List.nub (BC.free stmts)
              List.\\ (Map.keys nativeFunctions <> Map.keys globals <> args)

      modifyIORef' BC.assemblerState $ \s ->
        s
          { BC.metadata =
              IMap.insert
                i
                ( BC.FunctionMetaData
                    (length args)
                    (s.currentSize + 1)
                    (length (args <> freed))
                    (Map.fromList $ zip [0 ..] (args <> freed))
                )
                s.metadata
          , BC.locals = Map.fromList $ zip (args <> freed) [0 ..]
          , BC.globals = Map.insert n i globals
          }
      res <- concatMapM assembleStmt stmts

      modifyIORef' BC.assemblerState $ \s ->
        s {BC.locals = mempty, BC.currentSize = s.currentSize + length res}

      return
        ( BC.MakeAndStoreLambda i (length res) (length (args <> freed))
            : res
        )
    Nothing -> error $ "Function not declared: " <> show n
assembleProgram (Pre.DPDeclaration n e) = do
  e' <- assemble e
  BC.AssemblerState {BC.globals} <- readIORef BC.assemblerState
  case Map.lookup n globals of
    Just i -> do
      let res = e' ++ [BC.StoreGlobal i]
      return res
    Nothing -> error $ "Global variable not found: " <> show n
assembleProgram (Pre.DPMutDeclaration n e) = do
  e' <- assemble e
  BC.AssemblerState {BC.globals} <- readIORef BC.assemblerState
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
assembleProgram (Pre.DPNativeFunction fp n _ st) = do
  BC.AssemblerState {BC.nativeFunctions, BC.constants, BC.nativeLibraries} <-
    readIORef BC.assemblerState
  case Map.lookup n nativeFunctions of
    Just _ -> compilerError "Native function already declared"
    Nothing -> do
      i <- case Map.lookup (LString n) constants of
        Just i' -> pure i'
        Nothing -> do
          modifyIORef' BC.assemblerState $ \s ->
            s {BC.constants = Map.insert (LString n) (Map.size constants) constants}
          pure $ Map.size constants
      let path = toString fp
      let libIdx = case elemIndexAcc nativeLibraries (path, st) 0 of
            Just i' -> i'
            Nothing -> length nativeLibraries

      let lib = List.lookup (path, st) nativeLibraries
      funLibIdx <- case lib of
        Just l -> pure $ length l
        Nothing -> pure 0

      modifyIORef' BC.assemblerState $ \s ->
        s
          { BC.nativeFunctions =
              Map.insert
                n
                (funLibIdx, i, libIdx)
                nativeFunctions
          , BC.nativeLibraries = insertWith (<> [n]) nativeLibraries (path, st)
          }
      pure []

getNativeFunctions :: [Pre.DesugaredProgram] -> [Text]
getNativeFunctions = mapMaybe getNativeFunction
 where
  getNativeFunction (Pre.DPNativeFunction _ n _ _) = Just n
  getNativeFunction _ = Nothing

assembleUpdate :: Update -> IO [BC.Instruction]
assembleUpdate (UVariable n) = do
  BC.AssemblerState {BC.locals, BC.globals} <- readIORef BC.assemblerState
  case Map.lookup n locals of
    Just i -> pure [BC.LoadLocal i]
    Nothing -> case Map.lookup n globals of
      Just i -> pure [BC.LoadGlobal i]
      Nothing -> error $ "Variable not found: " <> show n
assembleUpdate (UProperty e p) = do
  e' <- assembleUpdate e
  pure $ e' ++ [BC.ListGet p]

runAssembler :: [Pre.DesugaredProgram] -> IO ([BC.Instruction], BC.AssemblerState)
runAssembler xs = do
  let freed = List.nub (BC.free xs) List.\\ getNativeFunctions xs
  modifyIORef' BC.assemblerState $ \s ->
    s
      { BC.globals = Map.fromList $ zip freed [0 ..]
      }
  is <- concatMapM assembleProgram xs
  s <- readIORef BC.assemblerState
  pure (is, s)

containsReturn :: [BC.Instruction] -> Bool
containsReturn = any isReturn
 where
  isReturn BC.Return = True
  isReturn BC.ReturnConst {} = True
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
  modifyIORef' BC.assemblerState $ \s -> s {BC.cwd = fp}
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
