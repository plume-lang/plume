module Plume.Compiler.Bytecode.Arithmetic where
import Plume.Compiler.Bytecode.Syntax qualified as BC
import Data.Map qualified as Map
import qualified Plume.Compiler.Desugaring.Syntax as Pre
import Plume.Syntax.Common (Literal(LInt))

type Instructions = [BC.Instruction]
type Callback = Pre.DesugaredExpr -> IO Instructions

compileFunction :: Callback -> Pre.DesugaredExpr -> IO Instructions
compileFunction callback (Pre.DEApplication f xs) 
  | f == "+::int" || f == "add_int" = add callback (Pre.DEApplication f xs)
  | f == "-::int" || f == "sub_int" = sub callback (Pre.DEApplication f xs)
  | f == "*::int" || f == "mul_int" = mul callback (Pre.DEApplication f xs)
  | f `elem` ["!=::int", "!=::bool", "!=::char", "!=::str"] = notEquals callback (Pre.DEApplication f xs)
  | f `elem` ["==::int", "==::bool", "==::char", "==::str"] = equals callback (Pre.DEApplication f xs)
compileFunction assemble (Pre.DEApplication f args) = do
  BC.AssemblerState {BC.nativeFunctions, BC.locals, BC.globals} <-
    readIORef BC.assemblerState
  args' <- concat <$> mapM assemble args
  pure $
    args' ++ case Map.lookup f globals of
      Just i -> [BC.CallGlobal i (length args)]
      Nothing -> case Map.lookup f locals of
        Just i -> [BC.CallLocal i (length args)]
        Nothing -> case Map.lookup f nativeFunctions of
          Just (funLibIdx, name, libAddr) -> do
            [BC.LoadNative name libAddr funLibIdx, BC.Call (length args)]
          _ -> error $ "Function not found: " <> show f
compileFunction _ _ = error "Invalid function application"

equals :: Callback -> Pre.DesugaredExpr -> IO Instructions
equals cb (Pre.DEApplication _ [a, b]) = do
  a' <- cb a
  b' <- cb b
  pure $ a' <> b' <> [BC.Compare BC.EqualTo]
equals _ _ = error "Invalid arguments for equals"

notEquals :: Callback -> Pre.DesugaredExpr -> IO Instructions
notEquals cb (Pre.DEApplication _ [a, b]) = do
  a' <- cb a
  b' <- cb b
  pure $ a' <> b' <> [BC.Compare BC.NotEqualTo]
notEquals _ _ = error "Invalid arguments for notEquals"

add :: Callback -> Pre.DesugaredExpr -> IO Instructions
add cb (Pre.DEApplication "+::int" [a, Pre.DELiteral l@(LInt _)]) = do
  a' <- cb a
  b' <- BC.assembleLit l
  pure $ a' <> [BC.AddConst b']
add cb (Pre.DEApplication "+::int" [Pre.DELiteral l@(LInt _), b]) = do
  a' <- BC.assembleLit l
  b' <- cb b
  pure $ b' <> [BC.AddConst a']
add cb (Pre.DEApplication "+::int" [a, b]) = do
  a' <- cb a
  b' <- cb b
  pure $ a' <> b' <> [BC.Add]
add cb (Pre.DEApplication "add_int" [a, Pre.DELiteral l@(LInt _)]) = do
  a' <- cb a
  b' <- BC.assembleLit l
  pure $ a' <> [BC.AddConst b']
add cb (Pre.DEApplication "add_int" [Pre.DELiteral l@(LInt _), b]) = do
  a' <- BC.assembleLit l
  b' <- cb b
  pure $ b' <> [BC.AddConst a']
add cb (Pre.DEApplication "add_int" [a, b]) = do
  a' <- cb a
  b' <- cb b
  pure $ a' <> b' <> [BC.Add]
add _ _ = error "Invalid arguments for add"

sub :: Callback -> Pre.DesugaredExpr -> IO Instructions
sub cb (Pre.DEApplication "-::int" [a, Pre.DELiteral l@(LInt _)]) = do
  a' <- cb a
  b' <- BC.assembleLit l
  pure $ a' <> [BC.SubConst b']
sub cb (Pre.DEApplication "-::int" [a, b]) = do
  a' <- cb a
  b' <- cb b
  pure $ a' <> b' <> [BC.Sub]
sub cb (Pre.DEApplication "sub_int" [a, Pre.DELiteral l@(LInt _)]) = do
  a' <- cb a
  b' <- BC.assembleLit l
  pure $ a' <> [BC.SubConst b']
sub cb (Pre.DEApplication "sub_int" [a, b]) = do
  a' <- cb a
  b' <- cb b
  pure $ a' <> b' <> [BC.Sub]
sub _ _ = error "Invalid arguments for sub"

mul :: Callback -> Pre.DesugaredExpr -> IO Instructions
mul cb (Pre.DEApplication "*::int" [a, Pre.DELiteral l@(LInt _)]) = do
  a' <- cb a
  b' <- BC.assembleLit l
  pure $ a' <> [BC.MulConst b']
mul cb (Pre.DEApplication "*::int" [Pre.DELiteral l@(LInt _), b]) = do
  a' <- BC.assembleLit l
  b' <- cb b
  pure $ b' <> [BC.MulConst a']
mul cb (Pre.DEApplication "*::int" [a, b]) = do
  a' <- cb a
  b' <- cb b
  pure $ a' <> b' <> [BC.Mul]
mul cb (Pre.DEApplication "mul_int" [a, Pre.DELiteral l@(LInt _)]) = do
  a' <- cb a
  b' <- BC.assembleLit l
  pure $ a' <> [BC.MulConst b']
mul cb (Pre.DEApplication "mul_int" [Pre.DELiteral l@(LInt _), b]) = do
  a' <- BC.assembleLit l
  b' <- cb b
  pure $ b' <> [BC.MulConst a']
mul cb (Pre.DEApplication "mul_int" [a, b]) = do
  a' <- cb a
  b' <- cb b
  pure $ a' <> b' <> [BC.Mul]
mul _ _ = error "Invalid arguments for mul"
