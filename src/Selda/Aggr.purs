module Selda.Aggr
  ( Aggr(..)
  , WrapWithAggr(..)
  , UnAggr(..)
  ) where

import Data.Symbol (SProxy)
import Heterogeneous.Mapping (class Mapping, class MappingWithIndex)
import Prim.TypeError (class Fail, Text, Beside)
import Selda.Col (Col)
import Unsafe.Coerce (unsafeCoerce)

newtype Aggr s a = Aggr (Col s a)

data WrapWithAggr = WrapWithAggr
instance wrapWithAggrInstance
    ∷ Mapping WrapWithAggr (Col s a) (Aggr s a)
  where
  mapping _ = Aggr

infixl 4 type Beside as <:>

data UnAggr = UnAggr
instance unAggrInstancedsa
    ∷ Fail (Text "field '"
        <:> Text sym
        <:> Text "' is not aggregated. Its type should be 'Aggr _ _'")
    ⇒ MappingWithIndex UnAggr (SProxy sym) (Col s a) c
  where
  mappingWithIndex _ _ _ = unsafeCoerce "failed with error message"
else instance unAggrInstance
    ∷ MappingWithIndex UnAggr (SProxy sym) (Aggr s a) (Col s a)
  where
  mappingWithIndex _ _ (Aggr col) = col
