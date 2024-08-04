module Plume.Compiler.Desugaring.Modules.ANF where

import Plume.Compiler.ClosureConversion.Syntax qualified as Pre
import Plume.Compiler.Desugaring.Modules.Switch
import Plume.Compiler.Desugaring.Monad
import Plume.Compiler.Desugaring.Syntax qualified as Post
import Control.Monad.Exception (compilerError)

desugarANF
  :: (IsToplevel, IsReturned, IsExpression, ShouldBeANF)
  -> DesugarModule Pre.ClosedExpr (ANFResult Post.DesugaredExpr)
desugarANF _ f (Pre.CEApplication x xs) = do
  (x', stmts1) <- f x
  (xs', stmts2) <- mapAndUnzipM f xs

  case x' of
    Post.DEVar name -> do
      let stmts' = stmts1 <> concat stmts2

      return (Post.DEApplication name xs', stmts')
    _ -> do
      fresh <- freshName
      let stmts' = stmts1 <> [Post.DSDeclaration fresh x'] <> concat stmts2

      return (Post.DEApplication fresh xs', stmts')
desugarANF t@(_, _, _, sba) f (Pre.CEDeclaration name expr body) = do
  (expr', stmt1) <- f expr
  (body', stmts2) <- desugarANF t f body

  fresh <- freshName

  let stmts =
        stmt1
          <> [Post.DSDeclaration name expr']
          <> stmts2
          <> [Post.DSDeclaration fresh body' | sba]

  return (if sba then Post.DEVar fresh else body', stmts)
desugarANF t f (Pre.CEMutDeclaration name expr body) = do
  (expr', stmt1) <- f expr
  (body', stmts2) <- desugarANF t f body

  fresh <- freshName

  let stmts =
        stmt1
          <> [Post.DSMutDeclaration name expr']
          <> stmts2
          <> [Post.DSDeclaration fresh body']

  return (Post.DEVar fresh, stmts)
desugarANF t f (Pre.CEMutUpdate name expr body) = do
  (expr', stmt1) <- f expr
  (body', stmts2) <- desugarANF t f body

  fresh <- freshName

  let stmts =
        stmt1
          <> [Post.DSMutUpdate name expr']
          <> stmts2
          <> [Post.DSDeclaration fresh body']

  return (Post.DEVar fresh, stmts)
desugarANF (isTop, _, _, _) f (Pre.CEConditionBranch e1 e2 e3) = do
  (e1', stmts1) <- f e1
  r1 <- f e2
  r2 <- f e3

  let shRet1 = shouldExprReturn e2
      shRet2 = shouldExprReturn e3

  let br1 = createBr r1 shRet1
      br2 = createBr r2 shRet2

  if isTop
    then do
      let br = Post.DEIf e1' br1 br2
      return (br, stmts1)
    else do
      let expr = Post.DSReturn
      let br = expr $ Post.DEIf e1' br1 br2
      return (Post.DEVar "nil", stmts1 <> [br])
 where
  createBr (e, st) isRet = st <> [(if isRet then Post.DSReturn else Post.DSExpr) e]
desugarANF _ _ _ =
  compilerError "Received incorrect expression, not an ANF convertible one"
