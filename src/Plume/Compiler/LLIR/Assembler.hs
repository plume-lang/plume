module Plume.Compiler.LLIR.Assembler where

import Plume.Compiler.LLIR.Syntax qualified as LLIR
import Plume.Compiler.Desugaring.Syntax qualified as Pre
import Plume.Compiler.ClosureConversion.Syntax qualified as Pre
import Plume.Syntax.Abstract (IsStandard)
import Control.Monad.IO
import GHC.IO qualified as IO
import Data.IntMap qualified as IntMap
import Data.Set qualified as Set
import Data.Map qualified as Map
import Plume.Syntax.Common qualified as Cmm

{-# NOINLINE natives #-}
natives :: IORef (Set Text)
natives = IO.unsafePerformIO $ newIORef Set.empty

{-# NOINLINE nativeFunctionsHandler #-}
nativeFunctionsHandler :: IORef (Map Pre.FunctionName (Pre.LibraryPath, IsStandard))
nativeFunctionsHandler = IO.unsafePerformIO $ newIORef Map.empty

{-# NOINLINE constantPool #-}
constantPool :: IORef (Map Cmm.Literal Int)
constantPool = IO.unsafePerformIO $ newIORef Map.empty

fetchConstant :: Cmm.Literal -> IO Int
fetchConstant lit = do
  cnst <- readIORef constantPool
  case Map.lookup lit cnst of
    Just i -> pure i
    Nothing -> do
      modifyIORef' constantPool (Map.insert lit (Map.size cnst))
      pure (Map.size cnst)

class Assemble a where
  assemble :: a -> IOReader (Set Text) LLIR.Program

instance Assemble a => Assemble [a] where
  assemble = fmap concat . mapM assemble

shouldNotBeLabel :: LLIR.Segment -> Bool
shouldNotBeLabel (LLIR.Function {}) = False
shouldNotBeLabel _ = True

extractFrom :: LLIR.Segment -> [LLIR.Instruction]
extractFrom (LLIR.Function {}) = error "Not implemented"
extractFrom (LLIR.Instruction instr) = [instr]

instance Assemble Pre.DesugaredProgram where
  assemble (Pre.DPFunction name args body) = do
    body' <- local (<> Set.fromList args) $ concat <$> mapM assemble body
    let body'' = filter shouldNotBeLabel body'
        finalBody = concatMap extractFrom body''
    pure [LLIR.Function name args finalBody]

  assemble (Pre.DPStatement stmt) = assemble stmt

  assemble (Pre.DPDeclaration name expr) = do
    expr' <- assemble expr
    pure (expr' <> LLIR.storeGlobal name)
  
  assemble (Pre.DPMutDeclaration name expr) = do
    expr' <- assemble expr
    pure (expr' <> [LLIR.Instruction LLIR.MakeMutable] <>  LLIR.storeGlobal name)

  assemble (Pre.DPMutUpdate update expr) = do
    update' <- assemble update
    expr' <- assemble expr
    pure (expr' <> update' <> [LLIR.Instruction LLIR.Update])

  assemble (Pre.DPNativeFunction path name _ isStd) = do
    modifyIORef' natives (Set.insert name)
    modifyIORef' nativeFunctionsHandler (Map.insert name (path, isStd))

    pure []

instance Assemble Pre.DesugaredStatement where
  assemble (Pre.DSExpr expr) = assemble expr

  assemble (Pre.DSDeclaration name expr) = do
    expr' <- assemble expr
    pure (expr' <> LLIR.storeLocal name)
  
  assemble (Pre.DSMutDeclaration name expr) = do
    expr' <- assemble expr
    pure (expr' <> [LLIR.Instruction LLIR.MakeMutable] <> LLIR.storeLocal name)
  
  assemble (Pre.DSMutUpdate update expr) = do
    update' <- assemble update
    expr' <- assemble expr
    pure (expr' <> update' <> [LLIR.Instruction LLIR.Update])
  
  assemble (Pre.DSReturn expr) = do
    expr' <- assemble expr
    pure (expr' <> [LLIR.Instruction LLIR.Return])

instance Assemble Pre.DesugaredExpr where
  assemble (Pre.DEVar name) = do
    locals <- ask
    if name `Set.member` locals
      then pure [LLIR.Instruction (LLIR.LoadLocal name)]
      else do
        nats <- readIORef natives
        if name `Set.member` nats
          then pure [LLIR.Instruction (LLIR.LoadNative name)]
          else pure [LLIR.Instruction (LLIR.LoadGlobal name)]
  
  assemble (Pre.DEApplication name args) = do
    args' <- concat <$> mapM assemble args
    pure (args' <> [LLIR.Instruction (LLIR.Call name)])
  
  assemble (Pre.DELiteral lit) = do
    i <- liftIO $ fetchConstant lit
    pure [LLIR.Instruction (LLIR.LoadConstant i)]
  
  assemble (Pre.DEList es) = do
    es' <- concat <$> mapM assemble es
    pure (es' <> [LLIR.Instruction (LLIR.MakeList (length es))])

  assemble (Pre.DEIndex list index) = do
    list' <- assemble list
    index' <- assemble index
    pure (list' <> index' <> [LLIR.Instruction LLIR.GetIndex])
  
  assemble (Pre.DEProperty expr index) = do
    expr' <- assemble expr
    pure (expr' <> [LLIR.Instruction (LLIR.ListGet index)])

  assemble (Pre.DEDictionary dict) = assemble (Pre.DEList (IntMap.elems dict))

  assemble (Pre.DEIf cond then' else') = do
    cond' <- assemble cond
    then'' <- concat <$> mapM assemble then'
    else'' <- concat <$> mapM assemble else'
    pure (cond' <> [LLIR.Instruction (LLIR.JumpElseRel (length then' + 1))] <> then'' <> [LLIR.Instruction (LLIR.JumpRel (length else' + 1))] <> else'')

  assemble (Pre.DETypeOf _) = undefined
  assemble (Pre.DEIsConstructor _ _) = undefined
  
  assemble (Pre.DEEqualsTo e1 e2) = do
    e1' <- assemble e1
    e2' <- assemble e2
    pure (e1' <> e2' <> [LLIR.Instruction (LLIR.Compare LLIR.EqualTo)])
  
  assemble (Pre.DEAnd e1 e2) = do
    e1' <- assemble e1
    e2' <- assemble e2
    pure (e1' <> e2' <> [LLIR.Instruction (LLIR.Compare LLIR.AndCmp)])
  
  assemble Pre.DESpecial = pure [LLIR.Instruction LLIR.Special]

  assemble (Pre.DESlice e i) = do
    e' <- assemble e
    pure (e' <> [LLIR.Instruction (LLIR.Slice i)])
  
  assemble (Pre.DEGreaterThan e i) = do
    e' <- assemble e
    i' <- liftIO $ fetchConstant (Cmm.LInt $ toInteger i)
    pure (e' <> LLIR.loadConstant i' <> [LLIR.Instruction (LLIR.Compare LLIR.GreaterThan)])
  
  assemble (Pre.DEListLength e) = do
    e' <- assemble e
    pure (e' <> [LLIR.Instruction LLIR.ListLength])
  
  assemble (Pre.DEUnMut e) = do
    e' <- assemble e
    pure (e' <> [LLIR.Instruction LLIR.UnMut])

instance Assemble Pre.Update where
  assemble (Pre.UVariable name) = do
    locals <- ask
    if name `Set.member` locals
      then pure [LLIR.Instruction (LLIR.LoadLocal name)]
      else pure [LLIR.Instruction (LLIR.LoadGlobal name)]
  
  assemble (Pre.UProperty u i) = do
    u' <- assemble u
    pure (u' <> [LLIR.Instruction (LLIR.ListGet i)])

runLLIRAssembler :: Assemble a => a -> IO LLIR.Program
runLLIRAssembler = flip runReaderT Set.empty . assemble