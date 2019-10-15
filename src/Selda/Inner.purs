module Selda.Inner
  ( Inner
  , OuterCols(..)
  ) where

import Data.Symbol (SProxy)
import Heterogeneous.Mapping (class MappingWithIndex)
import Prim.TypeError (class Fail, Beside, Text)
import Selda.Aggr (Aggr)
import Selda.Col (Col(..))
import Unsafe.Coerce (unsafeCoerce)

data Inner s

data OuterCols = OuterCols
instance failOuterCols
    ∷ Fail (Text "Nested query with an `Aggr` column: "
        <:> Text sym
        <:> Text ", expected `Col`")
    ⇒ MappingWithIndex OuterCols (SProxy sym) (Aggr s a) c
  where
  mappingWithIndex _ _ _ = unsafeCoerce "failed with error message"
else instance outercolsInstance
    ∷ MappingWithIndex OuterCols (SProxy sym) (Col (Inner s) a) (Col s a)
  where
  mappingWithIndex _ _ (Col e) = Col e
  
infixl 4 type Beside as <:>
