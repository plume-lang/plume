module Plume.Compiler.SSA.Bundler where

import Plume.Compiler.Desugaring.Syntax

bundleProg :: DesugaredProgram -> Maybe DesugaredProgram
bundleProg (DPDeclare n) = Just $ DPDeclare n
bundleProg (DPFunction name args stmts isAsync) =
  Just $ DPFunction name args stmts isAsync
bundleProg (DPStatement (DSDeclaration n e)) =
  Just $ DPDeclaration n e
bundleProg (DPStatement s) = Just $ DPStatement s
bundleProg (DPNativeFunction fp name arity st) =
  Just $ DPNativeFunction fp name arity st
bundleProg (DPDeclaration n e) =
  Just $ DPDeclaration n e
bundleProg (DPMutDeclaration n e) =
  Just $ DPMutDeclaration n e
bundleProg (DPMutUpdate n e) =
  Just $ DPMutUpdate n e

bundle :: [DesugaredProgram] -> [DesugaredProgram]
bundle [] = []
bundle xs = mapMaybe bundleProg xs