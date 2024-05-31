module Plume.Compiler.LLIR.Free where

import Plume.Compiler.Desugaring.Syntax qualified as Pre
import Plume.Compiler.ClosureConversion.Syntax (Update(..))
import Data.Set qualified as Set

type Reserved = Set Text

class Free a where
  free :: Reserved -> a -> Set Text

-- Used to get the name of a function, declaration or native function
class Name a where
  getNames :: a -> Set Text

instance (Free a) => Free [a] where
  free r = foldMap (free r)

instance Free Pre.DesugaredExpr where
  free res (Pre.DEVar x)
    | x `Set.notMember` res = Set.singleton x
    | otherwise = mempty
  free res (Pre.DEApplication f args)
    | f `Set.notMember` res = Set.singleton f <> foldMap (free res) args
    | otherwise = foldMap (free res) args
  free res (Pre.DEList es) = foldMap (free res) es
  free res (Pre.DEProperty e _) = free res e
  free res (Pre.DEIf e1 e2 e3) = free res e1 <> free res e2 <> free res e3
  free res (Pre.DETypeOf e) = free res e
  free res (Pre.DEIsConstructor e _) = free res e
  free res (Pre.DEEqualsTo e1 e2) = free res e1 <> free res e2
  free res (Pre.DEAnd e1 e2) = free res e1 <> free res e2
  free res (Pre.DEDictionary es) = foldMap (free res) es
  free _ _ = mempty

instance Free Update where
  free res (UVariable x) = Set.singleton x Set.\\ res
  free res (UProperty e _) = free res e

instance Free Pre.DesugaredStatement where
  free res (Pre.DSExpr e) = free res e
  free res (Pre.DSReturn e) = free res e
  free res (Pre.DSDeclaration n e) = Set.singleton n <> free res e
  free res (Pre.DSMutDeclaration n e) = Set.singleton n <> free res e
  free res (Pre.DSMutUpdate n e) = free res n <> free res e

instance Free Pre.DesugaredProgram where
  free _ (Pre.DPFunction name _ _ _) = Set.singleton name
  free res (Pre.DPStatement s) = free res s
  free _ (Pre.DPNativeFunction {}) = mempty
  free _ (Pre.DPDeclaration n _) = Set.singleton n
  free _ (Pre.DPMutDeclaration n _) = Set.singleton n
  free res (Pre.DPMutUpdate n _) = free res n
  free _ _ = mempty

instance Name Pre.DesugaredProgram where  
  getNames (Pre.DPFunction {}) = Set.empty
  getNames (Pre.DPDeclaration _ _) = Set.empty
  getNames (Pre.DPMutDeclaration _ _) = Set.empty
  getNames (Pre.DPMutUpdate _ _) = Set.empty
  getNames (Pre.DPNativeFunction _ name _ _) = Set.singleton name
  getNames (Pre.DPStatement _) = mempty
  getNames (Pre.DPDeclare n) = Set.singleton n

instance Name Update where
  getNames (UVariable x) = Set.singleton x
  getNames (UProperty e _) = getNames e

instance Name a => Name [a] where
  getNames = foldMap getNames