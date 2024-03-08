module Plume.Syntax.Translation.ConcreteToAbstract.Require where

import Control.Exception
import GHC.IO.Exception
import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Concrete qualified as CST
import Plume.Syntax.Translation.Generics

convertRequire :: Translator CST.Expression AST.Expression
convertRequire _ (CST.ERequire _) =
  throwIO (userError "ERequire not implemented")
convertRequire _ _ = error "Impossible happened"