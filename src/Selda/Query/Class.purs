module Selda.Query.Class where

import Selda.Query.Type (FullQuery)
import Type.Proxy (Proxy)

class GenericQuery b m s i o | i → o where
    genericQuery ∷ Proxy b → FullQuery s { | i } → m (Array { | o })
