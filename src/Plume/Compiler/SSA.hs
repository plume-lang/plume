module Plume.Compiler.SSA where

import Plume.Compiler.Desugaring.Syntax
import Plume.Compiler.SSA.Bundler
import Plume.Compiler.SSA.DeadCode

runSSA :: [DesugaredProgram] -> [DesugaredProgram]
runSSA xs = do
  let xs' = analyseDeadCode xs
  bundle xs'
