{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Plume.TypeChecker.Monad.Type.Scheme where

import Data.Map qualified as M
import Data.Set qualified as S
import Plume.Syntax.Internal.Pretty.ANSI
import Plume.TypeChecker.Monad.Substitution
import Plume.TypeChecker.Monad.Type

data Scheme
  = Forall [Int] Qualifier
  deriving (Show)

data Qualified
  = PlumeType `Has` Text
  deriving (Show, Eq)

data Qualifier
  = [Qualified] :=>: PlumeType
  deriving (Show)

instance ANSIPretty Scheme where
  ansiPretty (Forall vars t) =
    "forall" <+> hsep (map pretty vars) <> "." <+> ansiPretty t

instance ANSIPretty Qualified where
  ansiPretty (t `Has` s) = ansiPretty t <+> "extends" <+> pretty s

instance ANSIPretty Qualifier where
  ansiPretty (qs :=>: t) = hsep (map ansiPretty qs) <+> "=>" <+> ansiPretty t

instance Types Scheme where
  free (Forall vars t) = free t `S.difference` free vars
  apply s (Forall vars t) = Forall vars (apply (M.withoutKeys s (free vars)) t)

instance Types Qualified where
  free (t `Has` _) = free t
  apply s (t `Has` s') = apply s t `Has` s'

instance Types Qualifier where
  free (qs :=>: t) = free t `S.difference` free qs
  apply s (qs :=>: t) = apply s qs :=>: apply s t