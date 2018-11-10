module Selda.Query.Type
  ( Query(..)
  , GenState
  , initState
  , freshId
  , runQuery
  ) where

import Prelude

import Control.Monad.State (State, get, put, runState)
import Data.Tuple (Tuple)
import Prim.RowList (kind RowList)
import Selda.Expr (Expr)
import Selda.Query.Type (GenState)
import Selda.Table (AliasedTable)

type GenState = 
  { sources ∷ Array AliasedTable
  , restricts ∷ Array (Expr Boolean)
  , nextId ∷ Int
  }

newtype Query s a = Query (State GenState a)
derive newtype instance functorQuery ∷ Functor (Query s)
derive newtype instance applyQuery ∷ Apply (Query s)
derive newtype instance applicativeQuery ∷ Applicative (Query s)
derive newtype instance bindQuery ∷ Bind (Query s)
derive newtype instance monadQuery ∷ Monad (Query s)

initState ∷ GenState
initState = 
  { sources: []
  , restricts: []
  , nextId: 0
  }

freshId ∷ ∀ s. Query s Int
freshId = Query do
  st ← get
  put $ st { nextId = st.nextId + 1 }
  pure st.nextId

runQuery ∷ ∀ a. (∀ s. Query s a) → Tuple a GenState
runQuery (Query st) = runState st initState
