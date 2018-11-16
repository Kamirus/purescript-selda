module Selda.Query
  ( restrict
  , select
  ) where

import Prelude

import Control.Monad.State (get, put)
import Data.Array ((:))
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Selda.Col (class ToCols, Col(..), toCols)
import Selda.Query.Type (Query(..), freshId)
import Selda.Table (class TableColumns, Table(..), tableColumns)
import Type.Proxy (Proxy(..))
import Type.Row (RLProxy(..))

restrict ∷ ∀ s. Col s Boolean → Query s Unit
restrict (Col e) = Query do
  st ← get
  put $ st { restricts = e : st.restricts }

select
  ∷ ∀ s r rl res i il
  . RL.RowToList r rl
  ⇒ TableColumns rl i
  ⇒ RL.RowToList i il
  ⇒ ToCols s i il res
  ⇒ Table r → Query s (Record res)
select (Table { name }) = do
  id ← freshId
  st ← Query get
  let
    aliased = { name, alias: name <> "_" <> show id }
    i = tableColumns aliased (RLProxy ∷ RLProxy rl)
  Query $ put $ st { sources = aliased : st.sources }
  pure $ toCols (Proxy ∷ Proxy s) i (RLProxy ∷ RLProxy il)
