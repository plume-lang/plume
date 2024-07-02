module Language.Plume.Frontend.Module.Monad where
import GHC.IO qualified as IO
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.Plume.Syntax.HLIR qualified as HLIR
import Control.Monad.Except

type MonadResolution m = (MonadIO m, MonadError (String, HLIR.Position) m)

-- | Module unit
-- | A module unit is a file (resp. module) that contains a list of imports,
-- | a list of variables, a list of types, and a list of classes.
data ModuleUnit = MkModuleUnit
  { name :: FilePath
  , path :: FilePath
  , public :: Bool
  , imports :: [ModuleUnit]
  , -- Imported data
    variables :: Set Text
  , types :: Set Text
  }
  deriving (Show, Eq)

-- | Module state
data ModuleState = MkModuleState
  { initialPath :: FilePath
  , currentDirectory :: FilePath
  , resolved :: Map FilePath ModuleUnit
  , standardPath :: Maybe FilePath
  , boundArgs :: [Text]
  , modulePath :: Maybe FilePath
  }
  deriving (Show, Eq)

type ImportStack = [FilePath]

{-# NOINLINE importStack #-}
importStack :: IORef ImportStack
importStack = IO.unsafePerformIO . newIORef $ []

{-# NOINLINE moduleState #-}
moduleState :: IORef ModuleState
moduleState = IO.unsafePerformIO . newIORef $
  MkModuleState
    ""
    ""
    Map.empty
    Nothing
    ["+", "-", "/", "*", "and", "or", "!=", "=="]
    Nothing

{-# NOINLINE resultState #-}
resultState :: IORef [HLIR.AST "declaration"]
resultState = IO.unsafePerformIO . newIORef $ []

instance Semigroup ModuleUnit where
  (MkModuleUnit _ _ pub1 i1 v1 t1) <> (MkModuleUnit n2 p2 pub2 i2 v2 t2) =
    MkModuleUnit
      n2
      p2
      (pub1 || pub2)
      (i1 <> i2)
      (v1 <> v2)
      (t1 <> t2)

instance Monoid ModuleUnit where
  mempty =
    MkModuleUnit
      "" "" False []
      (Set.fromList
        [
          "+", "-", "/", "*", "and", "or", "!=", "=="
        ])
      (Set.fromList
        [
          "int", "float", "str", "char", "unit"
        ])
