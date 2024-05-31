module Plume.Compiler.SSA.DeadCode where

import Data.IntMap qualified as IMap
import Data.Set qualified as S
import Plume.Compiler.ClosureConversion.Free
import Plume.Compiler.Desugaring.Syntax

-- DEAD CODE ELIMINATION
--
-- For a given program, we want to remove all the statements that
-- are not reachable or are not used in the program.
-- To achive such a thing, we need to follow these steps:
--
-- - When dealing with a toplevel declaration, we need to compute
--   dead code elimination on the remaining toplevel statements and
--   then analyse the deadcode eliminated statements to get a fully
--   list of used variables.
--     - If the declaration name is not included in the used variables,
--       we can safely remove the declaration.
--     - Otherwise, we need to keep the declaration
--
-- - That's the same for functions, local functions and local
--   declarations.
--
-- HOW TO PERFORM DEAD CODE ANALYSIS
--
-- Analysis is performed by computing the free variables of a given
-- statement list or expression. We need to compute free variables on
-- list to keep track of the variables that may be definedin the given
-- list.
--
-- So freeing variable is just a way to keep track of the variables that
-- are not bound in the given list. Unbound variables may refer to
-- backward declarations.

removeNilReturn :: [DesugaredStatement] -> [DesugaredStatement]
removeNilReturn (DSReturn (DEVar "nil") : xs) = removeNilReturn xs
removeNilReturn (DSExpr (DEVar "nil") : xs) = removeNilReturn xs
removeNilReturn (x : xs) = x : removeNilReturn xs
removeNilReturn [] = []

instance Free DesugaredExpr where
  free (DEVar "nil") = S.empty
  free (DEVar x) = S.singleton x
  free (DEApplication f args) = S.singleton f <> free args
  free (DELiteral _) = S.empty
  free (DEList es) = free es
  free (DEProperty e _) = free e
  free (DETypeOf e) = free e
  free (DEIsConstructor e _) = free e
  free (DEEqualsTo e1 e2) = free e1 <> free e2
  free (DEAnd e1 e2) = free e1 <> free e2
  free (DEIf e1 e2 e3) = free e1 <> fst (freeStmtList e2) <> fst (freeStmtList e3)
  free (DEDictionary es) = free es
  free (DEIndex e1 e2) = free e1 <> free e2
  free DESpecial = S.empty
  free (DESlice e _) = free e
  free (DEGreaterThan e1 _) = free e1
  free (DEListLength e) = free e
  free (DEUnMut e) = free e

instance Free DesugaredStatement where
  free (DSExpr e) = free e
  free (DSReturn e) = free e
  free (DSDeclaration n e) = free e S.\\ S.singleton n
  free (DSMutDeclaration n e) = free e S.\\ S.singleton n
  free (DSMutUpdate n e) = free e S.\\ free n
  free (DSIf e1 e2 e3) = free e1 <> fst (freeStmtList e2) <> fst (freeStmtList e3)

type BoundVariables = Set Text
type DeclaredFunctions = Set Text
type FreeVariables = Set Text
type Variables = (BoundVariables, FreeVariables)

freeStmtList :: [DesugaredStatement] -> Variables
freeStmtList xs = go xs (S.empty, S.empty)
 where
  go
    :: [DesugaredStatement]
    -> Variables
    -> Variables
  go (DSExpr e : rest) s =
    let freeE = free e
     in go rest (first (S.union freeE) s)
  go (DSDeclaration n e : rest) s =
    let freeE = S.delete n $ free e
     in go rest (bimap (S.union freeE) (S.insert n) s)
  go (DSReturn e : rest) s =
    let freeE = free e
     in go rest (first (S.union freeE) s)
  go (DSMutDeclaration n e : rest) s =
    let freeE = S.delete n $ free e
     in go rest (bimap (S.union freeE) (S.insert n) s)
  go (DSMutUpdate n e : rest) s =
    let freeE = free e S.\\ free n
     in go rest (bimap (S.union freeE) (S.union (free n)) s)
  go (DSIf e1 e2 e3 : rest) s =
    let freeE = free e1
        (freeE2, bound2) = freeStmtList e2
        (freeE3, bound3) = freeStmtList e3
     in go rest (bimap (S.union freeE) (S.union bound2) s <> (freeE2 <> freeE3, bound3))
  go [] s = s

freeProgList :: [DesugaredProgram] -> FreeVariables
freeProgList xs = fst $ go xs (S.empty, S.empty)
 where
  go
    :: [DesugaredProgram]
    -> Variables
    -> Variables
  go (DPFunction n args stmts _ : rest) s =
    let (freeStmts, bound) = freeStmtList stmts
        freeE = freeStmts S.\\ (S.fromList args <> S.singleton n)
        bound' = S.insert n bound
     in go rest (bimap (S.union freeE) (S.union bound') s)
  go (DPStatement stmt : rest) s =
    let freeStmt = freeStmtList [stmt]
     in go rest (freeStmt <> s)
  go (DPNativeFunction _ n _ _ : rest) s = go rest (second (S.insert n) s)
  go (DPDeclare n : rest) s = go rest (second (S.insert n) s)
  go (DPDeclaration n e : rest) s =
    let freeE = free e S.\\ S.singleton n
     in go rest (bimap (S.union freeE) (S.insert n) s)
  go (DPMutDeclaration n e : rest) s =
    let freeE = free e S.\\ S.singleton n
     in go rest (bimap (S.union freeE) (S.insert n) s)
  go (DPMutUpdate n e : rest) s =
    let freeE = free e S.\\ free n
     in go rest (bimap (S.union freeE) (<> free n) s)
  go [] s = s

instance Free DesugaredProgram where
  free (DPFunction n args stmts _) = free stmts S.\\ (S.fromList args <> S.singleton n)
  free (DPStatement s) = free s
  free (DPNativeFunction {}) = S.empty
  free (DPDeclaration n e) = free e S.\\ S.singleton n
  free (DPMutDeclaration n e) = free e S.\\ S.singleton n
  free (DPMutUpdate n e) = free e S.\\ free n
  free (DPDeclare _) = S.empty

removeDeadCode
  :: BoundVariables
  -> DeclaredFunctions
  -> [DesugaredProgram]
  -> [DesugaredProgram]
removeDeadCode s d (DPDeclare n : rest) = do
  let rest' = removeDeadCode s (S.insert n d) rest
  DPDeclare n : rest'
removeDeadCode s d (DPFunction n args stmts async : rest) =
  let bound = S.fromList args <> S.singleton n
      rest' = removeDeadCode (S.insert n s) d rest
      freeVars = freeProgList rest'
      stmts' = removeDeadCodeStmt bound stmts
   in if n `S.member` freeVars || n `S.member` d
        then DPFunction n args (removeNilReturn stmts') async : rest'
        else rest'
removeDeadCode s d (DPStatement stmt : rest) =
  let (_, b) = freeStmtList [stmt]
      rest' = removeDeadCode s d rest
      stmt' = removeDeadCodeStmt s [stmt]
      freeVars' = freeProgList rest'
   in if null b || b `S.isSubsetOf` freeVars'
        then map DPStatement stmt' ++ rest'
        else rest'
removeDeadCode s d (DPDeclaration n e : rest) =
  let e' = maybeToList $ removeDeadCodeExpr s e
      rest' = removeDeadCode (S.insert n s) d rest
  --  freeVars = freeProgList rest'
  --  in if n `S.member` freeVars
  --       then (DPDeclaration n <$> e') <> rest'
  --       else rest'
      in (DPDeclaration n <$> e') <> rest'
removeDeadCode s d (DPNativeFunction fp n arity st : rest) =
  let rest' = removeDeadCode (S.insert n s) d rest
      -- freeVars = freeProgList rest'
   in DPNativeFunction fp n arity st : rest'
removeDeadCode s d (DPMutDeclaration n e : rest) =
  let e' = maybeToList $ removeDeadCodeExpr s e
      rest' = removeDeadCode (S.insert n s) d rest
  --  freeVars = freeProgList rest'
  --  in if n `S.member` freeVars
  --       then (DPMutDeclaration n <$> e') <> rest'
  --       else rest'
      in (DPMutDeclaration n <$> e') <> rest'
removeDeadCode s d (DPMutUpdate n e : rest) =
  let e' = maybeToList $ removeDeadCodeExpr s e
      n' = free n
      rest' = removeDeadCode (S.union n' s) d rest
   in (DPMutUpdate n <$> e') <> rest'
removeDeadCode _ _ [] = []

removeDeadCodeStmt
  :: BoundVariables
  -> [DesugaredStatement]
  -> [DesugaredStatement]
removeDeadCodeStmt s (DSExpr e : rest) =
  let e' = maybe [] (\ex -> [DSExpr ex]) $ removeDeadCodeExpr s e
      rest' = removeDeadCodeStmt s rest
   in e' <> rest'
removeDeadCodeStmt s (DSReturn e : _) = DSReturn <$> maybeToList (removeDeadCodeExpr s e)
removeDeadCodeStmt s (DSDeclaration n e : rest) =
  let e' = maybeToList $ removeDeadCodeExpr s e
      rest' = removeDeadCodeStmt (S.insert n s) rest
      (freeVars, b) = freeStmtList rest'
   in if n `S.member` freeVars && n `S.notMember` (b <> s)
        then (DSDeclaration n <$> e') <> rest'
        else rest'
removeDeadCodeStmt s (DSMutDeclaration n e : rest) =
  let e' = maybeToList $ removeDeadCodeExpr s e
      rest' = removeDeadCodeStmt (S.insert n s) rest
      (freeVars, b) = freeStmtList rest'
   in if n `S.member` freeVars && n `S.notMember` (b <> s)
        then (DSMutDeclaration n <$> e') <> rest'
        else rest'
removeDeadCodeStmt s (DSMutUpdate n e : rest) =
  let e' = maybeToList $ removeDeadCodeExpr s e
      rest' = removeDeadCodeStmt (S.union (free n) s) rest
   in (DSMutUpdate n <$> e') <> rest'
removeDeadCodeStmt s (DSIf e1 e2 e3 : rest) =
  let rest' = removeDeadCodeStmt s rest
      e2' = removeDeadCodeStmt s e2
      e3' = removeDeadCodeStmt s e3
   in [DSIf e1  e2' e3'] <> rest'
removeDeadCodeStmt _ [] = []

removeDeadCodeExpr :: BoundVariables -> DesugaredExpr -> Maybe DesugaredExpr
removeDeadCodeExpr _ (DEVar "nil") = Nothing
removeDeadCodeExpr b (DEIf e1 e2 e3) =
  let e1' = removeDeadCodeExpr b e1
      e2' = removeDeadCodeStmt b e2
      e3' = removeDeadCodeStmt b e3
   in DEIf <$> e1' <*> pure e2' <*> pure e3'
removeDeadCodeExpr b (DEApplication f args) =
  let args' = map (removeDeadCodeExpr b) args
   in DEApplication f <$> sequence args'
removeDeadCodeExpr b (DEEqualsTo e1 e2) =
  let e1' = removeDeadCodeExpr b e1
      e2' = removeDeadCodeExpr b e2
   in DEEqualsTo <$> e1' <*> e2'
removeDeadCodeExpr b (DEAnd e1 e2) =
  let e1' = removeDeadCodeExpr b e1
      e2' = removeDeadCodeExpr b e2
   in DEAnd <$> e1' <*> e2'
removeDeadCodeExpr b (DEIndex e1 e2) =
  let e1' = removeDeadCodeExpr b e1
      e2' = removeDeadCodeExpr b e2
   in DEIndex <$> e1' <*> e2'
removeDeadCodeExpr _ l@(DELiteral _) = Just l
removeDeadCodeExpr b (DEList es) =
  let es' = map (removeDeadCodeExpr b) es
   in DEList <$> sequence es'
removeDeadCodeExpr b (DEProperty e p) =
  let e' = removeDeadCodeExpr b e
   in DEProperty <$> e' <*> pure p
removeDeadCodeExpr b (DETypeOf e) =
  let e' = removeDeadCodeExpr b e
   in DETypeOf <$> e'
removeDeadCodeExpr b (DEIsConstructor e c) =
  let e' = removeDeadCodeExpr b e
   in DEIsConstructor <$> e' <*> pure c
removeDeadCodeExpr b (DEDictionary es) =
  let es' = IMap.map (removeDeadCodeExpr b) es
   in DEDictionary <$> sequence es'
removeDeadCodeExpr b (DESlice e s) =
  let e' = removeDeadCodeExpr b e
   in DESlice <$> e' <*> pure s
removeDeadCodeExpr b (DEGreaterThan e1 e2) =
  let e1' = removeDeadCodeExpr b e1
   in DEGreaterThan <$> e1' <*> pure e2
removeDeadCodeExpr b (DEListLength e) =
  let e' = removeDeadCodeExpr b e
   in DEListLength <$> e'
removeDeadCodeExpr b (DEUnMut e) = DEUnMut <$> removeDeadCodeExpr b e
removeDeadCodeExpr _ (DEVar x) = Just $ DEVar x
removeDeadCodeExpr _ DESpecial = Just DESpecial
