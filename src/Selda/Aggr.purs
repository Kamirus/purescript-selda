module Selda.Aggr
  ( Aggr(..)
  , WrapWithAggr(..)
  , UnAggr(..)
  ) where

import Heterogeneous.Mapping (class Mapping)
import Selda.Col (Col(..))
import Selda.Inner (Inner)

newtype Aggr s a = Aggr (Col s a)

data WrapWithAggr = WrapWithAggr
instance wrapWithAggrInstance
    ∷ Mapping WrapWithAggr (Col s a) (Aggr s a)
  where
  mapping _ = Aggr

data UnAggr = UnAggr
instance unAggrInstance ∷ Mapping UnAggr (Aggr (Inner s) a) (Col s a) where
  mapping _ (Aggr (Col e)) = Col e
