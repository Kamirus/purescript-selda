module Selda.Query
  ( restrict
  , select
  , leftJoin
  , WrapWithMaybe
  ) where

import Prelude

import Control.Monad.State (get, put)
import Data.Array ((:))
import Data.Maybe (Maybe)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Selda.Col (class ToCols, Col(..), toCols)
import Selda.Query.Type (Query(..), Source(..), freshId)
import Selda.Table (class TableColumns, Table(..), tableColumns)
import Type.Proxy (Proxy(..))
import Type.Row (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

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
  Query $ put $ st { sources = CrossJoin aliased : st.sources }
  pure $ toCols (Proxy ∷ Proxy s) i (RLProxy ∷ RLProxy il)

leftJoin
  ∷ ∀ r s res rl il i mres
  . RL.RowToList r rl
  ⇒ TableColumns rl i
  ⇒ RL.RowToList i il
  ⇒ ToCols s i il res
  ⇒ HMap WrapWithMaybe (Record res) (Record mres)
  ⇒ Table r → (Record res → Col s Boolean) → Query s (Record mres)
leftJoin (Table { name }) on = do
  id ← freshId
  st ← Query get
  let
    aliased = { name, alias: name <> "_" <> show id }
    i = tableColumns aliased (RLProxy ∷ RLProxy rl)
    res = toCols (Proxy ∷ Proxy s) i (RLProxy ∷ RLProxy il)
    Col e = on res
  Query $ put $ st { sources = LeftJoin aliased e : st.sources }
  pure $ hmap WrapWithMaybe res

data WrapWithMaybe = WrapWithMaybe
instance wrapWithMaybeInstance
    ∷ Mapping WrapWithMaybe (Col s a) (Col s (Maybe a))
  where
  mapping WrapWithMaybe = unsafeCoerce
