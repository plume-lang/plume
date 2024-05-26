module Plume.Compiler.SSA where

import Plume.Compiler.Desugaring.Syntax
import Plume.Compiler.SSA.Bundler
import Plume.Compiler.SSA.DeadCode

-- | SSA TRANSFORMATION
-- | SSA transformation is a step in the compilation process where the AST is
-- | transformed into a simpler form where we check for duplicatas in code, for
-- | unused variables, and for other optimizations.
-- |
-- | The SSA transformation is done in multiple steps:
-- |
-- |  - Remove dead code: This step removes all the dead code from the AST.
-- |
-- |  - Bundle: This step bundles the AST into a simpler form where we can
-- |    optimize the code. This step is now considered as useless as there is
-- |    no need for a main function in the LLIR representation.

runSSA :: [DesugaredProgram] -> [DesugaredProgram]
runSSA xs = do
  let xs' = removeDeadCode mempty mempty xs
  bundle xs'
