module Plume.TypeChecker.Monad.Type.Error where

import Control.Monad.Exception
import Data.Text qualified as T
import Plume.Syntax.Concrete (Position)
import Plume.Syntax.Internal.Pretty.ANSI
import Plume.TypeChecker.Monad.Type
import Prettyprinter.Render.Terminal
import Text.Megaparsec (SourcePos (..), unPos)

data TypeError
  = UnificationFail PlumeType PlumeType
  | InfiniteType Int PlumeType
  | UnboundVariable Text
  | UnificationMismatch [PlumeType] [PlumeType]
  | NotAFunction PlumeType
  | CompilerError Text
  | EmptyMatch
  | NoExtensionFound Text PlumeType
  | MultipleExtensionsFound Text [PlumeType]
  | NoGenericExtensionFound PlumeType
  | TypeMissing Text

instance (Throwable a) => Throwable [a] where
  showError [] = ""
  showError xs = case last' of
    Just x -> first' <> " and " <> showError x
    Nothing -> first'
   where
    init' = fromMaybe [] $ viaNonEmpty init xs
    last' = viaNonEmpty last xs
    first' = T.intercalate "," (map showError init')

instance Throwable PlumeType where
  showError t =
    renderStrict $
      layoutPretty defaultLayoutOptions $
        ansiPretty t

instance Throwable TypeError where
  showError (UnificationFail t1 t2) =
    "Cannot unify "
      <> showError t1
      <> " with "
      <> showError t2
  showError (InfiniteType i t) =
    "Infinite type " <> show i <> " = " <> showError t
  showError (UnboundVariable v) =
    "Unbound variable " <> show v
  showError (UnificationMismatch ts1 ts2) =
    "Unification mismatch "
      <> showError ts1
      <> " with "
      <> showError ts2
  showError (NotAFunction t) =
    "Not a function " <> showError t
  showError (CompilerError t) = "Compiler error " <> show t
  showError EmptyMatch = "Empty match"
  showError (NoExtensionFound n t) =
    "No extension found for " <> show n <> " on type " <> showError t
  showError (MultipleExtensionsFound n ts) =
    "Multiple extensions found for " <> show n <> " on types " <> showError ts
  showError (NoGenericExtensionFound t) =
    "No generic extension found for type " <> showError t
  showError (TypeMissing t) = "Type missing " <> show t <> " in type extension"

instance (Throwable a) => Throwable (a, Maybe Position) where
  showError (err, pos) =
    showError err <> case pos of
      Just p -> " at " <> showError p
      Nothing -> ""

instance Throwable Position where
  showError (SourcePos fp l1 c1, SourcePos {}) =
    toText fp <> ":" <> show (unPos l1) <> ":" <> show (unPos c1)
