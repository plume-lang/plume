{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Plume.TypeChecker.Monad.Type.Scheme where

import Data.Map qualified as M
import Data.Set qualified as S
import Plume.Syntax.Internal.Pretty.ANSI
import Plume.TypeChecker.Monad.Substitution
import Plume.TypeChecker.Monad.Type

data Scheme
  = Forall [PlumeGeneric] PlumeType
  deriving (Show)

instance ANSIPretty Scheme where
  ansiPretty (Forall vars t) =
    "forall" <+> hsep (map ansiPretty vars) <> "." <+> ansiPretty t

instance Types Scheme where
  free (Forall vars t) = free t `S.difference` free vars
  apply s (Forall vars t) = Forall vars (apply (M.withoutKeys s (free vars)) t)
