module Selda.Query
  ( restrict
  , select
  , leftJoin
  , leftJoin'
  , WrapWithMaybe
  ) where

import Prelude

import Control.Monad.State (get, put)
import Data.Array ((:))
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Selda.Col (class ExtractCols, class ToCols, Col(..), getCols, toCols)
import Selda.Query.Type (Query(..), SQL(..), Source(..), freshId, runQuery)
import Selda.Table (class TableColumns, Table(..), Alias, tableColumns)
import Type.Proxy (Proxy(..))
import Type.Row (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

restrict ∷ ∀ s. Col s Boolean → Query s Unit
restrict (Col e) = Query do
  st ← get
  put $ st { restricts = e : st.restricts }

select
  ∷ ∀ s r rl res i il
  . RL.RowToList r rl ⇒ TableColumns rl i ⇒ RL.RowToList i il ⇒ ToCols s i il res
  ⇒ Table r → Query s (Record res)
select table = do
  { res, sql } ← fromTable table
  st ← Query get
  Query $ put $ st { sources = Product sql : st.sources }
  pure res

leftJoin
  ∷ ∀ r s res rl il i mres
  . RL.RowToList r rl ⇒ TableColumns rl i ⇒ RL.RowToList i il ⇒ ToCols s i il res
  ⇒ HMap WrapWithMaybe (Record res) (Record mres)
  ⇒ Table r → (Record res → Col s Boolean) → Query s (Record mres)
leftJoin table on = do
  { res, sql } ← fromTable table
  let Col e = on res
  st ← Query get
  Query $ put $ st { sources = LeftJoin sql e : st.sources }
  pure $ hmap WrapWithMaybe res

leftJoin'
  ∷ ∀ s res resl mres
  . HMap WrapWithMaybe (Record res) (Record mres)
  ⇒ RL.RowToList res resl ⇒ ExtractCols res resl
  ⇒ (Record res → Col s Boolean) → Query s (Record res) → Query s (Record mres)
leftJoin' on q = do
  { res, sql } ← fromSubQuery q
  let Col e = on res
  st ← Query get
  Query $ put $ st { sources = LeftJoin sql e : st.sources }
  pure $ hmap WrapWithMaybe res

fromTable
  ∷ ∀ r s res rl il i
  . RL.RowToList r rl ⇒ TableColumns rl i ⇒ RL.RowToList i il ⇒ ToCols s i il res
  ⇒ Table r → Query s { res ∷ Record res , sql ∷ SQL }
fromTable (Table { name }) = do
  id ← freshId
  st ← Query get
  let
    aliased = { name, alias: name <> "_" <> show id }
    i = tableColumns aliased (RLProxy ∷ RLProxy rl)
    res = toCols (Proxy ∷ Proxy s) i (RLProxy ∷ RLProxy il)
  pure $ { res, sql: FromTable aliased }

data WrapWithMaybe = WrapWithMaybe
instance wrapWithMaybeInstance
    ∷ Mapping WrapWithMaybe (Col s a) (Col s (Maybe a))
  where
  mapping WrapWithMaybe = (unsafeCoerce ∷ Col s a → Col s (Maybe a))

subQueryAlias ∷ ∀ s. Query s Alias
subQueryAlias = do
  id ← freshId
  pure $ "sub" <> "_q" <> show id

fromSubQuery
  ∷ ∀ res s resl
  . RL.RowToList res resl ⇒ ExtractCols res resl
  ⇒ Query s (Record res) → Query s { res ∷ Record res, sql ∷ SQL }
fromSubQuery q = do
  let (Tuple res st) = runQuery q
  alias ← subQueryAlias
  pure $ { res, sql: SubQuery alias $ st { cols = getCols res } }
