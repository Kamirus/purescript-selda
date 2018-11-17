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
import Selda.Query.Type (Source(..), Query(..), SQL(..), freshId)
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
  . 
  RL.RowToList r rl
  ⇒ TableColumns rl i
  ⇒ RL.RowToList i il
  ⇒ ToCols s i il res
  ⇒ Table r → Query s (Record res)
select table = do
  { res, aliased } ← aux table
  st ← Query get
  Query $ put $ st { sources = Product aliased : st.sources }
  pure res

leftJoin
  ∷ ∀ r s res rl il i mres
  . RL.RowToList r rl
  ⇒ TableColumns rl i
  ⇒ RL.RowToList i il
  ⇒ ToCols s i il res
  ⇒ HMap WrapWithMaybe (Record res) (Record mres)
  ⇒ Table r → (Record res → Col s Boolean) → Query s (Record mres)
leftJoin table on = do
  { res, aliased } ← aux table
  let Col e = on res
  st ← Query get
  Query $ put $ st { sources = LeftJoin aliased e : st.sources }
  pure $ hmap WrapWithMaybe res

aux
  ∷ ∀ r s res rl il i
  . RL.RowToList r rl
  ⇒ TableColumns rl i
  ⇒ RL.RowToList i il
  ⇒ ToCols s i il res
  ⇒ Table r → Query s { res ∷ Record res , aliased ∷ SQL }
aux (Table { name }) = do
  id ← freshId
  st ← Query get
  let
    aliased = { name, alias: name <> "_" <> show id }
    i = tableColumns aliased (RLProxy ∷ RLProxy rl)
    res = toCols (Proxy ∷ Proxy s) i (RLProxy ∷ RLProxy il)
  pure $ { res, aliased: FromTable aliased }

data WrapWithMaybe = WrapWithMaybe
instance wrapWithMaybeInstance
    ∷ Mapping WrapWithMaybe (Col s a) (Col s (Maybe a))
  where
  mapping WrapWithMaybe = unsafeCoerce
