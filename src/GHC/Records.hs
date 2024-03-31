{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Records where

import Data.Foldable
import Language.Haskell.TH
import Prelude hiding (Type)

class HasField x r a | x r -> a where
  hasField :: r -> (a -> r, a)

getField :: forall x r a. (HasField x r a) => r -> a
getField = snd . hasField @x -- Note: a.x = is getField @"x" a.

setField :: forall x r a. (HasField x r a) => r -> a -> r
setField = fst . hasField @x -- Note : a{x = b} is setField @"x" a b.

deriveHasField :: Name -> Q [Dec]
deriveHasField name = do
  TyConI (DataD _ dataName gens _ [RecC _ fields] _) <- reify name
  let genNames = map (\case (PlainTV n _) -> n; KindedTV n _ _ -> n) gens
  let dataName' = createAppT dataName (map VarT genNames)
  pure $ mkInstance dataName' <$> fields
 where
  mkInstance :: Type -> VarBangType -> Dec
  mkInstance dataName (fieldName, _, fieldType) =
    InstanceD
      Nothing
      []
      (createAppT ''HasField [litT' fieldName, dataName, fieldType])
      [ FunD
          'hasField
          [ Clause
              []
              ( NormalB
                  ( LamE
                      [VarP r]
                      ( TupE
                          [ Just $
                              LamE [VarP a] (RecUpdE (VarE r) [(fieldName, VarE a)])
                          , Just $ AppE (VarE fieldName) (VarE r)
                          ]
                      )
                  )
              )
              []
          ]
      ]
   where
    r = mkName "r"
    a = mkName "a"
    litT' = LitT . StrTyLit . nameBase

createAppT :: Name -> [Type] -> Type
createAppT = foldl AppT . ConT
