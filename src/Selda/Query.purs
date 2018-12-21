module Selda.Query where

import Prelude

import Control.Monad.State (modify_)
import Data.Array ((:))
import Data.Exists (mkExists)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple(..), snd)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class Mapping, class MappingWithIndex, hmap, hmapWithIndex)
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Selda.Aggr (Aggr(..), UnAggr(..), WrapWithAggr(..))
import Selda.Col (class GetCols, class ToCols, Col(..), getCols, toCols)
import Selda.Expr (Expr(..))
import Selda.Inner (Inner, OuterCols(..))
import Selda.Query.Type (FullQuery(..), Order, Query(..), SQL(..), Source(..), freshId, runQuery)
import Selda.Table (class TableColumns, Alias, Column(..), Table(..), tableColumns)
import Type.Proxy (Proxy(..))
import Type.Row (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

selectFrom
  ∷ ∀ r s cols res
  . FromTable s r cols
  ⇒ Table r
  → ({ | cols } → Query s { | res })
  → FullQuery s { | res }
selectFrom table k = FullQuery $ crossJoin table >>= k

selectFrom_
  ∷ ∀ inner s resi reso
  . FromSubQuery s inner resi
  ⇒ FullQuery (Inner s) { | inner }
  → ({ | resi } → Query s { | reso })
  → FullQuery s { | reso }
selectFrom_ iq k = FullQuery $ crossJoin_ iq >>= k

restrict ∷ ∀ s. Col s Boolean → Query s Unit
restrict (Col e) = Query $ modify_ \st → st { restricts = e : st.restricts }

crossJoin ∷ ∀ s r res. FromTable s r res ⇒ Table r → Query s { | res }
crossJoin table = do
  { res, sql } ← fromTable table
  Query $ modify_ $ \st → st { sources = Product sql : st.sources }
  pure res

crossJoin_
  ∷ ∀ inner s res
  . FromSubQuery s inner res
  ⇒ FullQuery (Inner s) { | inner }
  → Query s { | res }
crossJoin_ iq = do
  let q = unwrap iq
  { res, sql } ← fromSubQuery q
  Query $ modify_ $ \st → st { sources = Product sql : st.sources }
  pure res

aggregate
  ∷ ∀ s aggr res
  . HMap UnAggr { | aggr } { | res }
  ⇒ Query s { | aggr }
  → Query s { | res }
aggregate q = map (hmap UnAggr) q

groupBy ∷ ∀ s a. Col s a → Query s (Aggr s a)
groupBy col@(Col e) = do
  Query $ modify_ \st → st { aggr = st.aggr <> [mkExists e] }
  pure $ Aggr col

groupBy'
  ∷ ∀ i o s
  . GetCols i
  ⇒ HMap WrapWithAggr { | i } { | o }
  ⇒ { | i }
  → Query s { | o }
groupBy' i = do
  let aggr = map snd $ getCols i
  Query $ modify_ \st → st { aggr = st.aggr <> aggr }
  pure $ hmap WrapWithAggr i

orderBy ∷ ∀ s a. Order → Aggr s a → Query s Unit
orderBy order (Aggr (Col e)) =
  Query $ modify_ \st → st { order = st.order <> [Tuple order $ mkExists e] }

limit ∷ ∀ s. Int → Query s Unit
limit i = Query $ modify_ $ _ { limit = Just i }

leftJoin
  ∷ ∀ r s res mres
  . FromTable s r res
  ⇒ HMap WrapWithMaybe { | res } { | mres }
  ⇒ Table r
  → ({ | res } → Col s Boolean)
  → Query s { | mres }
leftJoin table on = do
  { res, sql } ← fromTable table
  let Col e = on res
  Query $ modify_ \ st → st { sources = LeftJoin sql e : st.sources }
  pure $ hmap WrapWithMaybe res

-- | `leftJoin_ on q`
-- | run sub query `q`;
-- | with this execute `on` to get JOIN constraint;
-- | add sub query to sources;
-- | return previously mapped record with each value in Col wrapped in Maybe
-- | (because LEFT JOIN can return null for each column)
leftJoin_
  ∷ ∀ s res mres inner
  . FromSubQuery s inner res
  ⇒ HMap WrapWithMaybe { | res } { | mres }
  ⇒ ({ | res } → Col s Boolean)
  → FullQuery (Inner s) { | inner }
  → Query s { | mres }
leftJoin_ on iq = do
  let q = unwrap iq
  { res, sql } ← fromSubQuery q
  let Col e = on res
  Query $ modify_ \st → st { sources = LeftJoin sql e : st.sources }
  pure $ hmap WrapWithMaybe res

class FromTable s t c | s t → c where
  fromTable ∷ Table t → Query s { res ∷ { | c } , sql ∷ SQL }

instance tableToColsI
    ∷ ( RL.RowToList t tl
      , TableColumns tl i
      , ToCols s i c
      )
    ⇒ FromTable s t c
  where
  fromTable t@(Table { name }) = do
    id ← freshId
    let
      aliased = { name, alias: name <> "_" <> show id }
      i = tableColumns aliased (RLProxy ∷ RLProxy tl)
      res = toCols (Proxy ∷ Proxy s) i
    pure $ { res, sql: FromTable aliased }

data WrapWithMaybe = WrapWithMaybe
instance wrapWithMaybeLeaveMaybe
    ∷ Mapping WrapWithMaybe (Col s (Maybe a)) (Col s (Maybe a))
  where
  mapping _ = identity
else instance wrapWithMaybeInstance
    ∷ Mapping WrapWithMaybe (Col s a) (Col s (Maybe a))
  where
  mapping _ = (unsafeCoerce ∷ Col s a → Col s (Maybe a))

subQueryAlias ∷ ∀ s. Query s Alias
subQueryAlias = do
  id ← freshId
  pure $ "sub_q" <> show id

class FromSubQuery s inner res | s inner → res where
  fromSubQuery
    ∷ Query (Inner s) { | inner }
    → Query s { res ∷ { | res } , sql ∷ SQL , alias ∷ Alias }

instance fromSubQueryI 
    ∷ ( HMap OuterCols { | inner } { | res0 }
      , GetCols res0
      , HMapWithIndex SubQueryResult { | res0 } { | res }
      )
    ⇒ FromSubQuery s inner res
  where
  fromSubQuery q = do
    let (Tuple innerRes st) = runQuery q
    let res0 = hmap OuterCols innerRes
    alias ← subQueryAlias
    let res = createSubQueryResult alias res0
    pure $ { res, sql: SubQuery alias $ st { cols = getCols res0 }, alias }

-- | Outside of the subquery, every returned col (in SELECT ...) 
-- | (no matter if it's just a column of some table or expression or function or ...)
-- | is seen as a column of this subquery.
-- | So it can just be `<subquery alias>.<col alias>`.
-- | 
-- | Creates record of Columns with namespace set as subquery alias
-- | and column name as its symbol in record
-- | 
-- | ```purescript
-- | i ∷ { a ∷ Col s Int , b ∷ Col s String } = { a: lit 1, b: people.name }
-- | createSubQueryResult namespace i
-- | ==
-- | ({ a: ...{ namespace, name: "a" }, b: ...{ namespace, name: "b" } }
-- |   ∷ { a ∷ Col s Int , b ∷ Col s String })
-- | ```
createSubQueryResult
  ∷ ∀ i o
  . HMapWithIndex SubQueryResult { | i } { | o }
  ⇒ Alias → { | i } → { | o }
createSubQueryResult = hmapWithIndex <<< SubQueryResult

data SubQueryResult = SubQueryResult Alias
instance subQueryResultInstance
    ∷ IsSymbol sym
    ⇒ MappingWithIndex SubQueryResult (SProxy sym) (Col s a) (Col s a)
  where
  mappingWithIndex (SubQueryResult namespace) sym (Col _) = 
    Col $ EColumn $ Column { namespace, name: reflectSymbol sym }
