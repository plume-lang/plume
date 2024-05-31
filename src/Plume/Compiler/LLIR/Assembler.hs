module Plume.Compiler.LLIR.Assembler where

import Plume.Compiler.LLIR.Syntax qualified as LLIR
import Plume.Compiler.LLIR.Free qualified as LLIR
import Plume.Compiler.Desugaring.Syntax qualified as Pre
import Plume.Compiler.ClosureConversion.Syntax qualified as Pre
import Control.Monad.IO
import GHC.IO qualified as IO
import Data.IntMap qualified as IntMap
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.List qualified as List
import Plume.Syntax.Common qualified as Cmm
import Plume.Syntax.Translation.Generics

-- | LLIR ASSEMBLER
-- | The LLIR assembler is a step in the compilation process where the AST is
-- | transformed into a simpler form, the LLIR (Low-Level Intermediate
-- | Representation). It is a mix of high-level constructs (variables,
-- | functions...) and low-level constructs (instructions, jumps...).
-- |
-- | Transforming our code to LLIR is quite straightforward, as we only need to
-- | transform our AST into a sequence of instructions.
-- | But they are rules to respect:
-- |
-- |  - We need to keep track of the constants we use in our code, in order to
-- |    have a constant table to serialize
-- |
-- |  - We need to keep track of the global variables we use in our code, in
-- |    order to know if we load a local, global, or native variable
-- |
-- |  - We need to keep track of the native functions we use in our code, in
-- |    order to know if we call a local, global, or native function. We need it
-- |    too to know the address of the native function in the final binary and
-- |    the address too of the library.
-- |
-- |  - We need to know which functions are returning something, to know if we 
-- |    need to put a relative jump or not.

{-# NOINLINE natives #-}
natives :: IORef (Set Text)
natives = IO.unsafePerformIO $ newIORef Set.empty

{-# NOINLINE isFunctionCurrently #-}
isFunctionCurrently :: IORef Bool
isFunctionCurrently = IO.unsafePerformIO $ newIORef False

{-# NOINLINE nativeFunctionsHandler #-}
nativeFunctionsHandler :: IORef LLIR.Libraries
nativeFunctionsHandler = IO.unsafePerformIO $ newIORef Map.empty

{-# NOINLINE constantPool #-}
constantPool :: IORef (Map Cmm.Literal Int)
constantPool = IO.unsafePerformIO $ newIORef Map.empty

{-# NOINLINE globals #-}
globals :: IORef (Set Text)
globals = IO.unsafePerformIO $ newIORef Set.empty

fetchConstant :: Cmm.Literal -> IO Int
fetchConstant lit = do
  cnst <- readIORef constantPool
  case Map.lookup lit cnst of
    Just i -> pure i
    Nothing -> do
      modifyIORef' constantPool (Map.insert lit (Map.size cnst))
      pure (Map.size cnst)

class Assemble a where
  assemble :: a -> IOReader (Set Text) [LLIR.Segment]

instance Assemble a => Assemble [a] where
  assemble = fmap concat . mapM assemble

shouldNotBeLabel :: LLIR.Segment -> Bool
shouldNotBeLabel (LLIR.Function {}) = False
shouldNotBeLabel _ = True

extractFrom :: LLIR.Segment -> [LLIR.Instruction]
extractFrom (LLIR.Function {}) = error "Not implemented"
extractFrom (LLIR.Instruction instr) = [instr]

instance Assemble Pre.DesugaredProgram where
  assemble (Pre.DPFunction name args body _) = do
    writeIORef isFunctionCurrently True
    glbs <- readIORef globals
    ntvs <- readIORef natives
    let reserved = glbs <> ntvs
    
    let args' = Set.fromList args
        freed = LLIR.free reserved body
        env   = freed <> args'

    body' <- local (<> env) $ concat <$> mapM assemble body

    let localSpaceSize = Set.size env

    let body'' = filter shouldNotBeLabel body'
        finalBody = concatMap extractFrom body''

    let locals = List.nub $ args <> Set.toList freed
        localSpace = zip locals [0..]

    writeIORef isFunctionCurrently False

    pure [LLIR.Function name args localSpaceSize localSpace (finalBody <> [LLIR.ReturnUnit])]

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

  assemble (Pre.DPNativeFunction fp name _ isStd) = do
    modifyIORef' natives (Set.insert name)
    nativesHandlers <- readIORef nativeFunctionsHandler

    nameConstantIdx <- liftIO $ fetchConstant (Cmm.LString name)
    let libIdx = case Map.lookup fp nativesHandlers of
          Just (LLIR.MkNativeLibrary { LLIR.nativeAddressLibrary = addr }) ->
            addr
          Nothing -> Map.size nativesHandlers

    let funLibIdx = case Map.lookup fp nativesHandlers of
          Just l -> Map.size l.nativeFunctions
          Nothing -> 0

    let newLibFunction = LLIR.NativeFunction nameConstantIdx funLibIdx libIdx
    modifyIORef' nativeFunctionsHandler (\m -> do
        case Map.lookup fp m of
          Just l -> Map.insert fp (l { LLIR.nativeFunctions = Map.insert name newLibFunction (l.nativeFunctions) }) m
          Nothing -> Map.insert 
            fp 
            (LLIR.MkNativeLibrary fp libIdx isStd (Map.singleton name newLibFunction)) m
      )
    pure []

  assemble (Pre.DPDeclare name) = do
    modifyIORef' globals (Set.insert name)
    pure []

instance Assemble Pre.DesugaredStatement where
  assemble (Pre.DSExpr expr) = assemble expr

  assemble (Pre.DSDeclaration name expr) = do
    expr' <- assemble expr
    
    withinFunction <- readIORef isFunctionCurrently
    let fun = if withinFunction then LLIR.storeLocal else LLIR.storeGlobal

    pure (expr' <> fun name)
  
  assemble (Pre.DSMutDeclaration name expr) = do
    expr' <- assemble expr

    withinFunction <- readIORef isFunctionCurrently
    let fun = if withinFunction then LLIR.storeLocal else LLIR.storeGlobal

    pure (expr' <> [LLIR.Instruction LLIR.MakeMutable] <> fun name)
  
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
    glbs <- readIORef globals
    nats <- readIORef globals
    if name `Set.member` locals
      then pure [LLIR.Instruction (LLIR.LoadLocal name)]
    else if name `Set.member` glbs
      then pure [LLIR.Instruction (LLIR.LoadGlobal name)]
    else if name `Set.member` nats
      then pure [LLIR.Instruction (LLIR.LoadNative name)]
    else pure [LLIR.Instruction (LLIR.LoadGlobal name)]
  
  assemble (Pre.DEApplication name args) = do
    args' <- concat <$> mapM assemble args
    let argsLength = length args
    locals <- ask
    nats <- readIORef natives
    glbs <- readIORef globals

    if name `Set.member` locals
      then pure $ args' ++ [LLIR.Instruction (LLIR.CallLocal name argsLength)]
    else if name `Set.member` glbs
      then pure $ args' ++ [LLIR.Instruction (LLIR.CallGlobal name argsLength)]
    else if name `Set.member` nats
      then pure $ args' 
                ++ LLIR.instr (LLIR.LoadNative name) 
                ++ LLIR.instr (LLIR.Call argsLength)
    else pure $ args' ++ [LLIR.Instruction (LLIR.CallGlobal name argsLength)]

  assemble (Pre.DELiteral lit) = do
    i <- liftIO $ fetchConstant lit
    pure [LLIR.Instruction (LLIR.LoadConstant i)]
  
  assemble (Pre.DEList es) = do
    es' <- concat <$> mapM assemble es
    pure (es' <> [LLIR.Instruction (LLIR.MakeList (length es))])

  assemble (Pre.DEIndex list (Pre.DELiteral (Cmm.LInt i))) = do
    list' <- assemble list
    pure (list' <> LLIR.instr (LLIR.ListGet (fromInteger i)))

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
    then'' <- concatMapM assemble then'
    else'' <- concatMapM assemble else'

    let doesElseReturn = doesReturn else''
        thenJumpAddr   = length then'' + if doesElseReturn then 1 else 2
        thenBranch     = LLIR.instr (LLIR.JumpElseRel thenJumpAddr)

        elseJumpAddr   = if doesElseReturn then length else'' else length else'' + 1
        elseBranch     = [LLIR.Instruction (LLIR.JumpRel elseJumpAddr) | not doesElseReturn]

    pure (cond' <> thenBranch <> then'' <> elseBranch <> else'')

  assemble (Pre.DETypeOf _) = error "TypeOf is not implemented"
  assemble (Pre.DEIsConstructor _ _) = error "IsConstructor is not implemented"
  
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
  
  assemble (Pre.UProperty u i) = case readEither (toString i) of
    Right i' -> do
      u' <- assemble u
      pure (u' <> [LLIR.Instruction (LLIR.ListGet i')])
    Left _ -> error "Property index must be an integer"

isReturn :: LLIR.Instruction -> Bool
isReturn LLIR.Return = True
isReturn (LLIR.ReturnConst _) = True
isReturn _ = False

doesReturn :: [LLIR.Segment] -> Bool
doesReturn (LLIR.Instruction x:_) | isReturn x = True
doesReturn (_:xs) = doesReturn xs
doesReturn [] = False

runLLIRAssembler :: (Assemble a, LLIR.Free a, LLIR.Name a) => a -> IO LLIR.Program
runLLIRAssembler xs = do
  writeIORef globals (getGlobals xs)
  writeIORef constantPool Map.empty
  writeIORef natives (LLIR.getNames xs)
  writeIORef nativeFunctionsHandler Map.empty

  xs' <- runReaderT (assemble xs) Set.empty

  nats <- readIORef nativeFunctionsHandler
  constants' <- Map.toList <$> readIORef constantPool
  let constants'' = IntMap.fromList (invertAList constants')
  pure (xs', nats, IntMap.elems constants'')

getGlobals :: (LLIR.Name a, LLIR.Free a) => a -> Set Text
getGlobals x = do
  let globals' = LLIR.free mempty x
      natives' = LLIR.getNames x
    in globals' Set.\\ natives'

getNativeFunctions :: LLIR.Libraries -> Map Text LLIR.NativeFunction
getNativeFunctions = Map.unions . map LLIR.nativeFunctions . Map.elems

invertAList :: [(b, a)] -> [(a, b)]
invertAList = map (\(a, b) -> (b, a))
