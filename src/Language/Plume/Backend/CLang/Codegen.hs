module Language.Plume.Backend.CLang.Codegen where

import Language.Plume.Backend.CLang.Monad qualified as M
import Language.Plume.Syntax.LLIR qualified as LLIR
import Language.Plume.Syntax.CLang qualified as CLang
import Data.Char qualified as Char
import Data.Set qualified as Set
import Text.Printf

isIdent :: Char -> Bool
isIdent x = Char.isLetter x || x == '_' || x == '$'

varify :: String -> String
varify "main" = "main"
varify x = "$" ++ concatMap (\x' -> if isIdent x' then [x'] else show (ord x')) x

encodeUnicode16 :: String -> String
encodeUnicode16 = concatMap escapeChar
  where
    escapeChar c
      | c == '\"' = "\\\""
      | c == '\'' = "\\\'"
      | ' ' <= c && c <= 'z' = [c]
      | Char.isPrint c = [c]
      | otherwise = printf "\\u%04x" (fromEnum c)

fromType :: M.MonadCLang m => LLIR.PlumeType -> m Text
fromType LLIR.MkTyInt = pure "int"
fromType LLIR.MkTyFloat = pure "float"
fromType LLIR.MkTyChar = pure "char"
fromType LLIR.MkTyString = pure "char*"
fromType LLIR.MkTyUnit = pure "void"
fromType LLIR.MkTyAny = pure "void*"
fromType LLIR.MkTyBool = pure "int"
fromType (LLIR.MkTyId name) = do
  let name' = varify (toString name)
  pure $ "struct " <> fromString name'
fromType (args LLIR.:->: ret) = do
  args' <- mapM fromType args
  ret' <- fromType ret
  
  name <- M.freshSymbol "fun"

  modifyIORef' M.resultState (<> [CLang.MkDeclTypedef name args' ret'])
  pure name
fromType (LLIR.MkTyVar tv) = do
  v <- readIORef tv

  case v of
    LLIR.Unbound _ _ -> pure "void*"
    LLIR.Link ty -> fromType ty
fromType _ = pure "void"

generateToplevel :: M.MonadCLang m => LLIR.LLIR "declaration" -> m ()
generateToplevel (LLIR.MkDeclFunction name _ args ret (LLIR.MkExprBlock exprs)) = M.reset $ do
  ret' <- fromType ret
  args' <- mapM (\(LLIR.MkAnnotation name' ty) -> do
    let argName = fromString . varify . toString $ name'
    ty' <- fromType ty
    pure $ CLang.MkAnnotation argName ty') args
  let fnName = fromString . varify . toString $ name

  exprs' <- mapM generate exprs

  modifyIORef' M.resultState (<> [CLang.MkDeclFunction fnName args' ret' exprs'])
generateToplevel (LLIR.MkDeclFunction name _ args ret body) = M.reset $ do
  b <- generate body
  
  ret' <- fromType ret
  args' <- mapM (\(LLIR.MkAnnotation name' ty) -> do
    let argName = fromString . varify . toString $ name'
    ty' <- fromType ty
    pure $ CLang.MkAnnotation argName ty') args
  let fnName = fromString . varify . toString $ name

  modifyIORef' M.resultState (<> [CLang.MkDeclFunction fnName args' ret' [
       CLang.MkExprReturn b
    ]])
generateToplevel (LLIR.MkDeclNative name _ args ret) = do
  args' <- mapM fromType args
  ret' <- fromType ret 
  modifyIORef' M.reserved (Set.insert name)
  modifyIORef' M.resultState (<> [CLang.MkDeclExtern name args' ret'])
generateToplevel (LLIR.MkDeclStruct name annots) = do
  let stName = fromString . varify . toString $ name
  annots' <- mapM (\(LLIR.MkAnnotation name' ty) -> do
    ty' <- fromType ty
    pure $ CLang.MkAnnotation name' ty') annots
  modifyIORef' M.resultState (<> [CLang.MkDeclStruct stName annots'])

generate :: M.MonadCLang m => LLIR.LLIR "expression" -> m (CLang.CLang "expression")
generate (LLIR.MkExprLiteral lit) = pure $ CLang.MkExprLiteral lit
generate (LLIR.MkExprVariable name _) = do
  reserved <- readIORef M.reserved
  
  if Set.member name reserved
    then pure $ CLang.MkExprVariable name 
    else pure $ CLang.MkExprVariable (fromString . varify . toString $ name)
generate (LLIR.MkExprCall (LLIR.MkExprVariable "+" _) [x, y] _) = do
  x' <- generate x
  y' <- generate y
  pure $ CLang.MkExprBinary x' "+" y'
generate (LLIR.MkExprCall (LLIR.MkExprVariable "-" _) [x, y] _) = do
  x' <- generate x
  y' <- generate y
  pure $ CLang.MkExprBinary x' "-" y'
generate (LLIR.MkExprCall (LLIR.MkExprVariable "*" _) [x, y] _) = do
  x' <- generate x
  y' <- generate y
  pure $ CLang.MkExprBinary x' "*" y'
generate (LLIR.MkExprCall (LLIR.MkExprVariable "/" _) [x, y] _) = do
  x' <- generate x
  y' <- generate y
  pure $ CLang.MkExprBinary x' "/" y'
generate (LLIR.MkExprCall (LLIR.MkExprVariable "==" _) [x, y] _) = do
  x' <- generate x
  y' <- generate y
  pure $ CLang.MkExprBinary x' "==" y'
generate (LLIR.MkExprCall (LLIR.MkExprVariable "!=" _) [x, y] _) = do
  x' <- generate x
  y' <- generate y
  pure $ CLang.MkExprBinary x' "!=" y'
generate (LLIR.MkExprCall callee args _) = do
  callee' <- generate callee
  args' <- mapM generate args
  pure $ CLang.MkExprCall callee' args'
generate (LLIR.MkExprIf cond then' else' _) = do
  cond' <- generate cond
  then'' <- generate then'
  else'' <- generate else'
  pure $ CLang.MkExprIf cond' [then''] [else'']
generate (LLIR.MkExprLet name ty expr _) = do
  expr' <- generate expr
  let name' = fromString . varify . toString $ name
  ty' <- fromType ty
  pure $ CLang.MkExprLet name' ty' expr'
generate (LLIR.MkExprBlock _) = error "todo"
generate (LLIR.MkExprField e name _ _) = do
  e' <- generate e
  pure $ CLang.MkExprField e' name
generate (LLIR.MkExprStruct name annots) = do
  let name' = fromString . varify . toString $ name
  annots' <- mapM (\(LLIR.MkAnnotation ann e) -> do
    e' <- generate e
    pure $ CLang.MkAnnotation ann e') annots
  pure $ CLang.MkExprStruct name' annots'
generate (LLIR.MkExprCast e t) = do
  e' <- generate e
  t' <- fromType t
  pure $ CLang.MkExprCast e' t'
generate (LLIR.MkExprReturn e) = do
  e' <- generate e
  pure $ CLang.MkExprReturn e'
generate (LLIR.MkExprRef e _) = do
  e' <- generate e
  pure $ CLang.MkExprRef e'
generate (LLIR.MkExprPtrField e name _) = do
  e' <- generate e
  let unrefAccess = CLang.MkExprField (CLang.MkExprDeref e') name
  pure unrefAccess
generate (LLIR.MkExprTernary c t e _) = do
  c' <- generate c
  t' <- generate t
  e' <- generate e
  pure $ CLang.MKExprTernary c' t' e'

runCLang :: M.MonadCLang m => [LLIR.LLIR "declaration"] -> m [CLang.CLang "declaration"]
runCLang decls = M.reset $ do
  mapM_ generateToplevel decls
  readIORef M.resultState
