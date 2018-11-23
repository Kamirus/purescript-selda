module Selda.Inner
  ( Inner
  , outer
  , OuterCols
  ) where

import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Selda.Col (Col(..))

data Inner s

data OuterCols = OuterCols
instance outercolsInstance
    ∷ Mapping OuterCols (Col (Inner s) a) (Col s a)
  where
  mapping _ (Col e) = Col e

outer
  ∷ ∀ inner res
  . HMap OuterCols (Record inner) (Record res)
  ⇒ Record inner → Record res
outer i = hmap OuterCols i
