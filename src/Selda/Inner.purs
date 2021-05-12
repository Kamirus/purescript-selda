module Selda.Inner
  ( Inner
  , OuterCols(..)
  ) where

import Heterogeneous.Mapping (class MappingWithIndex)
import Prim.TypeError (class Fail, Beside, Text)
import Selda.Aggr (Aggr)
import Selda.Col (Col(..))
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

data Inner :: forall k. k -> Type
data Inner s

data OuterCols = OuterCols
instance failOuterCols
    ∷ Fail (Text "Nested query with an `Aggr` column: "
        <:> Text sym
        <:> Text ", expected `Col`")
    ⇒ MappingWithIndex OuterCols (Proxy sym) (Aggr s a) c
  where
  mappingWithIndex _ _ _ = unsafeCoerce "failed with error message"
else instance outercolsInstance
    ∷ MappingWithIndex OuterCols (Proxy sym) (Col (Inner s) a) (Col s a)
  where
  mappingWithIndex _ _ (Col e) = Col e
  
infixl 4 type Beside as <:>
