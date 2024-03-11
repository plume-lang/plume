module Plume.Syntax.Concrete.Internal.Row where

import Plume.Syntax.Common
import Plume.Syntax.Concrete

-- IsRow is a typeclass utility used to check if a polymorphic type "a"
-- is a row type. This is used notably in the prettyprinter, to display
-- records in a generic way.
class IsRow a where
  -- Check if the polymorphic row is empty
  isRowEmpty :: a -> Bool

  -- Check if the polymorphic row is an extension
  isRowExtend :: a -> Bool

  -- Extract all extension informations from the row
  -- Note that the value of type a should be a row type, it must
  -- contain a label, a value and an optional rest of the row.
  extractExtend :: a -> ([Annotation a], a)

instance IsRow PlumeType where
  isRowEmpty TRowEmpty = True
  isRowEmpty _ = False

  isRowExtend (TRowExtend {}) = True
  isRowExtend _ = False

  extractExtend (TRowExtend label val r') = ((label :@: val) : names, rest')
   where
    (names, rest') = extractExtend r'
  extractExtend e = ([], e)

instance IsRow Expression where
  isRowEmpty ERowEmpty = True
  isRowEmpty _ = False

  isRowExtend (ERowExtension {}) = True
  isRowExtend _ = False

  extractExtend (ERowExtension label val r') = ((label :@: val) : names, rest')
   where
    (names, rest') = extractExtend r'
  extractExtend e = ([], e)
