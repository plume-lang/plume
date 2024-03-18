module Plume.Compiler.TypeErasure.DynamicDispatch.RTTI where

import Plume.TypeChecker.Monad.Type

class RTTI a where
  rtti :: a -> Text

instance RTTI PlumeType where
  rtti (TApp t xs) = rtti t <> next
   where
    xs' = rtti xs
    next = if xs' == mempty then mempty else "_" <> xs'
  rtti (TVar _) = mempty
  rtti (TId t) = t

instance (RTTI a) => RTTI [a] where
  rtti xs = mconcat $ intersperse "_" $ map rtti xs