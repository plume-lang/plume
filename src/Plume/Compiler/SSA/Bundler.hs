module Plume.Compiler.SSA.Bundler where

import Plume.Compiler.Desugaring.Syntax

bundleProg :: DesugaredProgram -> Maybe DesugaredProgram
bundleProg (DPFunction name args stmts) =
  Just $ DPFunction name args stmts
bundleProg (DPStatement (DSDeclaration n e)) =
  Just $ DPDeclaration n e
bundleProg (DPStatement s) = Just $ DPStatement s
bundleProg (DPNativeFunction fp name arity) =
  Just $ DPNativeFunction fp name arity
bundleProg (DPDeclaration n e) =
  Just $ DPDeclaration n e

bundle :: [DesugaredProgram] -> [DesugaredProgram]
bundle [] = []
bundle xs = mapMaybe bundleProg xs