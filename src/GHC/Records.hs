{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module GHC.Records where

class HasField x r a | x r -> a where
  hasField :: r -> (a -> r, a)

getField :: forall x r a. (HasField x r a) => r -> a
getField = snd . hasField @x -- Note: a.x = is getField @"x" a.

setField :: forall x r a. (HasField x r a) => r -> a -> r
setField = fst . hasField @x -- Note : a{x = b} is setField @"x" a b.
