module Selda.Aggr
  ( Aggr(..)
  , WrapWithAggr(..)
  , UnAggr(..)
  , class Coerce, unsafeFromCol
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

-- | Overloading utility for common operations on `Col` and `Aggr`
class Coerce col where
  -- | Either an identity or `Aggr` constructor.
  -- | Can be used when it's safe to operate on both `Col` and `Aggr`.
  -- | Not every `Col` can be safely coerced to `Aggr`.
  unsafeFromCol ∷ ∀ s a. Col s a → col s a

instance coerceCol ∷ Coerce Col where unsafeFromCol x = x

instance coerceAggr ∷ Coerce Aggr where unsafeFromCol = Aggr

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
