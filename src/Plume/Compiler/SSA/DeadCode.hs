module Plume.Compiler.SSA.DeadCode where

import Data.Set qualified as S
import Plume.Compiler.ClosureConversion.Free
import Plume.Compiler.Desugaring.Syntax

removeDeadCodeStmt
  :: S.Set Text -> DesugaredStatement -> Maybe DesugaredStatement
removeDeadCodeStmt _ (DSExpr e) = case e of
  DEVar _ -> Nothing
  DELiteral _ -> Nothing
  DEList _ -> Nothing
  DEProperty _ _ -> Nothing
  DEDictionary _ -> Nothing
  DETypeOf _ -> Nothing
  DEIsConstructor _ _ -> Nothing
  DEEqualsTo _ _ -> Nothing
  DEAnd _ _ -> Nothing
  _ -> Just $ DSExpr e
removeDeadCodeStmt _ (DSReturn (DEVar "nil")) = Nothing
removeDeadCodeStmt _ e = Just e

doesContainReturn :: DesugaredStatement -> Bool
doesContainReturn (DSReturn _) = True
doesContainReturn (DSConditionBranch _ e1 e2) =
  any doesContainReturn e1 || any doesContainReturn e2
doesContainReturn _ = False

removeDeadCodeProg :: DesugaredProgram -> Maybe DesugaredProgram
removeDeadCodeProg (DPFunction name args stmts) = case stmts' of
  [] -> Nothing
  _ ->
    if any doesContainReturn stmts'
      then Just $ DPFunction name args stmts'
      else case last' of
        DSExpr e -> Just $ DPFunction name args (init' <> [DSReturn e])
        DSReturn _ -> Just $ DPFunction name args stmts'
        _ -> Just $ DPFunction name args (stmts' <> [DSReturn (DEVar "nil")])
 where
  stmts' = mapMaybe (removeDeadCodeStmt mempty) stmts
  last' = fromMaybe (error "Cannot have empty body") $ viaNonEmpty last stmts'
  init' = fromMaybe [] $ viaNonEmpty init stmts'
removeDeadCodeProg (DPStatement s) = removeDeadCodeStmt mempty s >>= Just . DPStatement
removeDeadCodeProg (DPNativeFunction name arity) = Just $ DPNativeFunction name arity
removeDeadCodeProg z@DPDeclaration {} = Just z

removeDeadCode :: [DesugaredProgram] -> [DesugaredProgram]
removeDeadCode = mapMaybe removeDeadCodeProg

instance Free DesugaredExpr where
  free (DEVar x) = S.singleton x
  free (DEApplication f args) = S.singleton f <> free args
  free (DELiteral _) = S.empty
  free (DEList es) = free es
  free (DEProperty e _) = free e
  free (DETypeOf e) = free e
  free (DEIsConstructor e _) = free e
  free (DEEqualsTo e1 e2) = free e1 <> free e2
  free (DEAnd e1 e2) = free e1 <> free e2
  free (DEIf e1 e2 e3) = free e1 <> free e2 <> free e3
  free (DEDictionary es) = free es
  free (DEIndex e1 e2) = free e1 <> free e2

instance Free DesugaredStatement where
  free (DSExpr e) = free e
  free (DSReturn e) = free e
  free (DSDeclaration n e) = free e S.\\ S.singleton n
  free (DSConditionBranch e1 e2 e3) = free e1 <> free e2 <> free e3

instance Free DesugaredProgram where
  free (DPFunction n args stmts) = free stmts S.\\ (S.fromList args <> S.singleton n)
  free (DPStatement s) = free s
  free (DPNativeFunction _ _) = S.empty
  free (DPDeclaration n e) = free e S.\\ S.singleton n

analyseDeadCodeStmt
  :: S.Set Text -> [DesugaredStatement] -> [DesugaredStatement]
analyseDeadCodeStmt freed (DSDeclaration name expr : xs) =
  case free xs <> freed of
    freeExpr
      | S.member name freeExpr ->
          DSDeclaration name expr : analyseDeadCodeStmt (S.insert name freed) xs
    _ -> xs
analyseDeadCodeStmt freed (x : xs) = case removeDeadCodeStmt freed x of
  Just x' -> x' : analyseDeadCodeStmt freed xs
  Nothing -> analyseDeadCodeStmt freed xs
analyseDeadCodeStmt _ [] = []

analyseDeadCode :: [DesugaredProgram] -> [DesugaredProgram]
analyseDeadCode (DPFunction name args stmts : xs) =
  case free xs of
    freeProg
      | S.member name freeProg -> do
          let fun = removeDeadCodeProg (DPFunction name args stmts')
          case fun of
            Just fun' -> fun' : analyseDeadCode xs
            Nothing -> analyseDeadCode xs
     where
      stmts' = analyseDeadCodeStmt (S.fromList (name : args)) stmts
    _ -> analyseDeadCode xs
analyseDeadCode (DPStatement s : xs) = case removeDeadCodeStmt mempty s of
  Just s' -> DPStatement s' : analyseDeadCode xs
  Nothing -> analyseDeadCode xs
analyseDeadCode (x : xs) = x : analyseDeadCode xs
analyseDeadCode [] = []