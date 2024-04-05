{-# LANGUAGE LambdaCase #-}

module Plume.Compiler.Desugaring.Modules.ANF where

import Data.Set qualified as S
import Plume.Compiler.ClosureConversion.Syntax qualified as Pre
import Plume.Compiler.Desugaring.Modules.Switch
import Plume.Compiler.Desugaring.Monad
import Plume.Compiler.Desugaring.Syntax qualified as Post

desugarANF
  :: (IsToplevel, IsReturned, IsExpression)
  -> DesugarModule Pre.ClosedExpr (ANFResult Post.DesugaredExpr)
desugarANF _ f (Pre.CEApplication x xs) = do
  (x', stmts1) <- f x
  (xs', stmts2) <- mapAndUnzipM f xs

  nativeFuns <- readIORef nativeFunctions
  (xs'', stmts3) <-
    mapAndUnzipM
      ( \case
          Post.DEApplication n args
            | n `S.notMember` nativeFuns -> do
                new <- freshName
                return (Post.DEVar new, [Post.DSDeclaration new (Post.DEApplication n args)])
          _x -> return (_x, [])
      )
      xs'

  case x' of
    Post.DEVar name -> do
      let stmts' = stmts1 <> concat stmts2 <> concat stmts3

      return (Post.DEApplication name xs'', stmts')
    _ -> do
      fresh <- freshName
      let stmts' = stmts1 <> [Post.DSDeclaration fresh x'] <> concat stmts2 <> concat stmts3

      return (Post.DEApplication fresh xs'', stmts')
desugarANF t f (Pre.CEDeclaration name expr body) = do
  (expr', stmt1) <- f expr
  (body', stmts2) <- desugarANF t f body

  fresh <- freshName

  let stmts =
        stmt1
          <> [Post.DSDeclaration name expr']
          <> stmts2
          <> [Post.DSDeclaration fresh body']

  return (Post.DEVar fresh, stmts)
desugarANF (isNotTop, isReturned, _) f (Pre.CEConditionBranch e1 e2 e3) = do
  (e1', stmts1) <- f e1
  r1@(e2', stmts2) <- f e2
  r2@(e3', stmts3) <- f e3

  if (not (null stmts2) || not (null stmts3))
    && isNotTop
    && not isReturned
    then do
      let br1 = createBr r1
      let br2 = createBr r2
      let br = Post.DSExpr $ Post.DEIf e1' br1 br2
      return (Post.DEVar "nil", stmts1 <> [br])
    else do
      let stmts = stmts1 <> stmts2 <> stmts3
      return (Post.DEIf e1' [Post.DSExpr e2'] [Post.DSExpr e3'], stmts)
 where
  createBr (e, st) = st <> [Post.DSReturn e]
desugarANF _ _ _ =
  error "Received incorrect expression, not an ANF convertible one"
