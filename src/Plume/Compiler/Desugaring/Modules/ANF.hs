{-# LANGUAGE LambdaCase #-}

module Plume.Compiler.Desugaring.Modules.ANF where

import Data.Set qualified as S
import Plume.Compiler.Desugaring.Monad
import Plume.Compiler.Desugaring.Syntax qualified as Post
import Plume.Compiler.TypeErasure.Syntax qualified as Pre

desugarANF :: DesugarModule Pre.UntypedExpr (ANFResult Post.DesugaredExpr)
desugarANF f (Pre.UEApplication x xs) = do
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
desugarANF f (Pre.UEDeclaration name expr body) = do
  (expr', stmt1) <- f expr
  (body', stmts2) <- desugarANF f body

  fresh <- freshName

  let stmts =
        stmt1
          <> [Post.DSDeclaration name expr']
          <> stmts2
          <> [Post.DSDeclaration fresh body']

  return (Post.DEVar fresh, stmts)
desugarANF _ _ = error "test"
