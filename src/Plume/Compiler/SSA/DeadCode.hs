module Plume.Compiler.SSA.DeadCode where

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
  free (DEGreaterThan e1 e2) = free e1 <> free e2
  free (DEListLength e) = free e

instance Free DesugaredStatement where
  free (DSExpr e) = free e
  free (DSReturn e) = free e
  free (DSDeclaration n e) = free e S.\\ S.singleton n

type BoundVariables = Set Text
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
  go [] s = s

freeProgList :: [DesugaredProgram] -> FreeVariables
freeProgList xs = fst $ go xs (S.empty, S.empty)
 where
  go
    :: [DesugaredProgram]
    -> Variables
    -> Variables
  go (DPFunction n args stmts : rest) s =
    let (freeStmts, bound) = freeStmtList stmts
        freeE = freeStmts S.\\ (S.fromList args <> S.singleton n)
        bound' = S.insert n bound
     in go rest (bimap (S.union freeE) (S.union bound') s)
  go (DPStatement stmt : rest) s =
    let freeStmt = freeStmtList [stmt]
     in go rest (freeStmt <> s)
  go (DPNativeFunction _ n _ : rest) s = go rest (second (S.insert n) s)
  go (DPDeclaration n e : rest) s =
    let freeE = free e S.\\ S.singleton n
     in go rest (bimap (S.union freeE) (S.insert n) s)
  go [] s = s

instance Free DesugaredProgram where
  free (DPFunction n args stmts) = free stmts S.\\ (S.fromList args <> S.singleton n)
  free (DPStatement s) = free s
  free (DPNativeFunction {}) = S.empty
  free (DPDeclaration n e) = free e S.\\ S.singleton n

removeDeadCode
  :: BoundVariables
  -> [DesugaredProgram]
  -> [DesugaredProgram]
removeDeadCode s (DPFunction n args stmts : rest) =
  let bound = S.fromList args <> S.singleton n
      rest' = removeDeadCode (S.insert n s) rest
      freeVars = freeProgList rest'
      stmts' = removeDeadCodeStmt bound stmts
   in if n `S.member` freeVars
        then DPFunction n args (removeNilReturn stmts') : rest'
        else rest'
removeDeadCode s (DPStatement stmt : rest) =
  let (_, b) = freeStmtList [stmt]
      rest' = removeDeadCode s rest
      freeVars' = freeProgList rest'
   in if null b || b `S.isSubsetOf` freeVars'
        then DPStatement stmt : rest'
        else rest'
removeDeadCode s (DPDeclaration n e : rest) =
  let rest' = removeDeadCode (S.insert n s) rest
      freeVars = freeProgList rest'
   in if n `S.member` freeVars
        then DPDeclaration n e : rest'
        else rest'
removeDeadCode s (DPNativeFunction fp n arity : rest) =
  let rest' = removeDeadCode (S.insert n s) rest
      freeVars = freeProgList rest'
   in if n `S.member` freeVars
        then DPNativeFunction fp n arity : rest'
        else rest'
removeDeadCode _ [] = []

removeDeadCodeStmt
  :: BoundVariables
  -> [DesugaredStatement]
  -> [DesugaredStatement]
removeDeadCodeStmt s (DSExpr e : rest) =
  let rest' = removeDeadCodeStmt s rest
   in DSExpr e : rest'
removeDeadCodeStmt _ (DSReturn e : _) = [DSReturn e]
removeDeadCodeStmt s (DSDeclaration n e : rest) =
  let rest' = removeDeadCodeStmt (S.insert n s) rest
      (freeVars, b) = freeStmtList rest'
   in if n `S.member` freeVars && n `S.notMember` (b <> s)
        then DSDeclaration n e : rest'
        else rest'
removeDeadCodeStmt _ [] = []
