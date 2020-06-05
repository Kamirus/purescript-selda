module Selda.Aggr
  ( Aggr(..)
  , WrapWithAggr(..)
  , UnAggr(..)
  ) where

import Data.HeytingAlgebra (class HeytingAlgebra, ff, implies, not, tt, (&&), (||))
import Data.Symbol (SProxy)
import Heterogeneous.Mapping (class Mapping, class MappingWithIndex)
import Prim.TypeError (class Fail, Text, Beside)
import Selda.Col (Col)
import Unsafe.Coerce (unsafeCoerce)

newtype Aggr s a = Aggr (Col s a)

instance heytingAlgebraAggr ∷ HeytingAlgebra (Aggr s Boolean) where
  ff = Aggr ff
  tt = Aggr tt
  implies (Aggr a) (Aggr b) = Aggr (a `implies` b)
  conj (Aggr a) (Aggr b) = Aggr (a && b)
  disj (Aggr a) (Aggr b) = Aggr (a || b)
  not (Aggr e) = Aggr (not e)

data WrapWithAggr = WrapWithAggr
instance wrapWithAggrInstance
    ∷ Mapping WrapWithAggr (Col s a) (Aggr s a)
  where
  mapping _ = Aggr

infixl 4 type Beside as <:>

data UnAggr = UnAggr
instance failUnAggr
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
