module Selda.Query where

import Prelude

import Data.Array ((:))
import Data.Exists (mkExists)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple(..), snd)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class Mapping, class MappingWithIndex, hmap, hmapWithIndex)
import Prim.RowList as RL
import Selda.Aggr (Aggr(..), UnAggr(..), WrapWithAggr(..))
import Selda.Col (class GetCols, class ToCols, Col(..), getCols, toCols)
import Selda.Expr (Expr(..), UnExp(..), UnOp(..))
import Selda.Inner (Inner, OuterCols(..))
import Selda.Query.Type (FullQuery(..), GenState(..), JoinType(..), Order, QBinOp(..), Query, SQL(..), Source(..), freshId, modify_, runFullQuery)
import Selda.Table (class TableColumns, Alias, Column(..), Table(..), tableColumns)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | `selectFrom table k` creates a fully described query which is a valid SQL query.
-- | 
-- | It starts with the following SQL query scheme:
-- | 
-- | ```sql
-- | SELECT <res>
-- | FROM <table>
-- | <query_description>
-- | ```
-- | 
-- | The continuation `k` is executed with a record of columns from the `table`
-- | The rest of the query - `<query_description>` - is specified using 
-- | actions in the `Query` monad (such as `restrict`)
-- | 
-- | Query result <res> is specified as a result of the continuation `k`
-- | 
-- | EXAMPLE:
-- | ```purescript
-- | selectFrom tableWithIdAndName \r → do
-- |   restrict $ r.id .== lit 17
-- |   pure { nameOfaUserWithId17: r.name }
-- | ```
-- | 
-- | SQL equivalent:
-- | ```SQL
-- | SELECT r.name AS nameOfaUserWithId17
-- | FROM tableWithIdAndName r
-- | WHERE r.id == 17
-- | ```
selectFrom
  ∷ ∀ r s cols res
  . FromTable s r cols
  ⇒ Table r
  → ({ | cols } → Query s { | res })
  → FullQuery s { | res }
selectFrom table k = FullQuery do
  { res, sql } ← fromTable table
  modify_ \st → st { source = From sql }
  k res

-- | Similar to the `selectFrom` but starts a query from the given sub query (not a table)
-- | See the `selectFrom` documentation for more details.
selectFrom_
  ∷ ∀ inner s resi reso
  . FromSubQuery s inner resi
  ⇒ FullQuery (Inner s) { | inner }
  → ({ | resi } → Query s { | reso })
  → FullQuery s { | reso }
selectFrom_ iq k = FullQuery do
  { res, sql } ← fromSubQuery iq
  modify_ \st → st { source = From sql }
  k res

-- | `restrict condition` adds the `condition` to the SQL `WHERE` clause.
-- | Multiple `restrict` operations are joined with `AND`
restrict ∷ ∀ s. Col s Boolean → Query s Unit
restrict (Col e) = modify_ \st → st { restricts = e : st.restricts }

-- | `having condition` adds the `condition` to the SQL `HAVING` clause.
-- | Multiple `having` operations are joined with `AND`
having ∷ ∀ s. Aggr s Boolean → Query s Unit
having (Aggr (Col e)) = modify_ \st → st { havings = e : st.havings }

-- | `nutNull col` adds to the WHERE clause that col is not null
-- | and returns the coerced column.
notNull ∷ ∀ s a. Col s (Maybe a) → Query s (Col s a)
notNull col@(Col e) = do 
  let
    notNullCol = Col $ EUnOp $ mkExists $ UnExp (IsNotNull identity) e
    fromMaybeCol = (unsafeCoerce ∷ Col s (Maybe a) → Col s a)
  restrict notNullCol
  pure $ fromMaybeCol col

-- | `nutNull_ aggr` adds to the HAVING clause that aggregate expression `aggr`
-- | is not null and returns the coerced column.
notNull_ ∷ ∀ s a. Aggr s (Maybe a) → Query s (Aggr s a)
notNull_ aggr@(Aggr (Col e)) = do 
  let
    notNullAggr = Aggr $ Col $ EUnOp $ mkExists $ UnExp (IsNotNull identity) e
    fromMaybeAggr = (unsafeCoerce ∷ Aggr s (Maybe a) → Aggr s a)
  having notNullAggr
  pure $ fromMaybeAggr aggr

-- | `crossJoin table predicate` is equivalent to `CROSS JOIN <table> ON <predicate>`.
-- | Returns the columns from the joined table.
crossJoin ∷ ∀ s r res. FromTable s r res ⇒ Table r → Query s { | res }
crossJoin table = do
  { res, sql } ← fromTable table
  modify_ \st → st { source = CrossJoin st.source sql }
  pure res

-- | `crossJoin_ subquery predicate` is equivalent to `CROSS JOIN <subquery> ON <predicate>`.
-- | Returns the columns from the joined subquery.
crossJoin_
  ∷ ∀ inner s res
  . FromSubQuery s inner res
  ⇒ FullQuery (Inner s) { | inner }
  → Query s { | res }
crossJoin_ iq = do
  { res, sql } ← fromSubQuery iq
  modify_ \st → st { source = CrossJoin st.source sql }
  pure res

distinct
  ∷ ∀ s r
  . FullQuery s { | r }
  → FullQuery s { | r }
distinct (FullQuery q) = FullQuery do
  modify_ \st → st { distinct = true }
  q

aggregate
  ∷ ∀ s aggr res
  . HMapWithIndex UnAggr { | aggr } { | res }
  ⇒ FullQuery s { | aggr }
  → FullQuery s { | res }
aggregate q = map (hmapWithIndex UnAggr) q

groupBy ∷ ∀ s a. Col s a → Query s (Aggr s a)
groupBy col@(Col e) = do
  modify_ \st → st { aggr = st.aggr <> [mkExists e] }
  pure $ Aggr col

groupBy'
  ∷ ∀ i o s
  . GetCols i
  ⇒ HMap WrapWithAggr { | i } { | o }
  ⇒ { | i }
  → Query s { | o }
groupBy' i = do
  let aggr = map snd $ getCols i
  modify_ \st → st { aggr = st.aggr <> aggr }
  pure $ hmap WrapWithAggr i

orderBy ∷ ∀ s a. Order → Col s a → Query s Unit
orderBy order (Col e) =
  modify_ \st → st { order = st.order <> [Tuple order $ mkExists e] }

limit ∷ ∀ s. Int → Query s Unit
limit i = modify_ $ _ { limit = Just i }

-- | `innerJoin table predicate` is equivalent to `JOIN <table> ON <predicate>`.
-- | Returns the columns from the joined table.
innerJoin
  ∷ ∀ r s res
  . FromTable s r res
  ⇒ Table r
  → ({ | res } → Col s Boolean)
  → Query s { | res }
innerJoin table on = do
  { res, sql } ← fromTable table
  let Col e = on res
  modify_ \ st → st { source = JoinOn InnerJoin st.source sql e }
  pure res

-- | `innerJoin_ predicate subquery` is equivalent to `JOIN <subquery> ON <predicate>`.
-- | Returns the columns from the joined subquery.
innerJoin_
  ∷ ∀ s res inner
  . FromSubQuery s inner res
  ⇒ ({ | res } → Col s Boolean)
  → FullQuery (Inner s) { | inner }
  → Query s { | res }
innerJoin_ on iq = do
  { res, sql } ← fromSubQuery iq
  let Col e = on res
  modify_ \st → st { source = JoinOn InnerJoin st.source sql e }
  pure res

-- | `leftJoin table predicate` is equivalent to `LEFT JOIN <table> ON <predicate>`.
-- | Returns the columns from the joined table.
-- | These columns become nullable due to the LEFT JOIN semantics
-- | hence they are wrapped in Maybe.
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
  modify_ \ st → st { source = JoinOn LeftJoin st.source sql e }
  pure $ hmap WrapWithMaybe res

-- | `leftJoin_ predicate subquery` is equivalent to `LEFT JOIN <subquery> ON <predicate>`.
-- | Returns the columns from the joined subquery.
-- | These columns become nullable due to the LEFT JOIN semantics
-- | hence they are wrapped in Maybe.
leftJoin_
  ∷ ∀ s res mres inner
  . FromSubQuery s inner res
  ⇒ HMap WrapWithMaybe { | res } { | mres }
  ⇒ ({ | res } → Col s Boolean)
  → FullQuery (Inner s) { | inner }
  → Query s { | mres }
leftJoin_ on iq = do
  { res, sql } ← fromSubQuery iq
  let Col e = on res
  modify_ \st → st { source = JoinOn LeftJoin st.source sql e }
  pure $ hmap WrapWithMaybe res

type CombineQuery
  = ∀ s r inner i o
  . FromSubQuery s inner i
  ⇒ HMapWithIndex SubQueryResult { | i } { | o }
  ⇒ FullQuery (Inner s) { | inner }
  → FullQuery (Inner s) { | inner }
  → ({ | o } → Query s { | r })
  → FullQuery s { | r }

union ∷ CombineQuery
union = combineWith Union

unionAll ∷ CombineQuery
unionAll = combineWith UnionAll

intersect ∷ CombineQuery
intersect = combineWith Intersect

except ∷ CombineQuery
except = combineWith Except

combineWith
  ∷ ∀ s r inner i o
  . FromSubQuery s inner i
  ⇒ HMapWithIndex SubQueryResult { | i } { | o }
  ⇒ QBinOp
  → FullQuery (Inner s) { | inner }
  → FullQuery (Inner s) { | inner }
  → ({ | o } → Query s { | r })
  → FullQuery s { | r }
combineWith op q1 q2 k = FullQuery do
  r1 ← fromSubQuery q1
  r2 ← fromSubQuery q2
  alias ← freshId <#> \id → "comb_q" <> show id
  modify_ \st → st { source = Combination op r1.st r2.st alias }
  -- records `r1.res` and `r2.res` are identical, so we use either
  k $ createSubQueryResult alias r1.res

class FromTable s t c | s t → c where
  fromTable ∷ Table t → Query s { res ∷ { | c } , sql ∷ SQL }

instance tableToColsI
    ∷ ( RL.RowToList t tl
      , TableColumns tl i
      , ToCols s i c
      )
    ⇒ FromTable s t c
  where
  fromTable = case _ of
    Table { name } → 
      go name \alias → { body: name <> " " <> alias, alias }
    Source aliasPrefix aliasToBody →
      go aliasPrefix \alias → { body: aliasToBody $ Just alias, alias }
    where
    go aliasPrefix aliasToAliased = do
      id ← freshId
      let
        aliased = aliasToAliased $ aliasPrefix <> "_" <> show id
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

class FromSubQuery s inner res | inner → s res where
  fromSubQuery
    ∷ FullQuery (Inner s) { | inner }
    → Query s { res ∷ { | res } , sql ∷ SQL , alias ∷ Alias , st ∷ GenState }

instance fromSubQueryI 
    ∷ ( HMapWithIndex OuterCols { | inner } { | res0 }
      , GetCols res0
      , HMapWithIndex SubQueryResult { | res0 } { | res }
      )
    ⇒ FromSubQuery s inner res
  where
  fromSubQuery q = do
    let (Tuple innerRes (GenState st)) = runFullQuery q
    let res0 = hmapWithIndex OuterCols innerRes
    alias ← subQueryAlias
    let res = createSubQueryResult alias res0
    let genState = wrap $ st { cols = getCols res0 }
    pure $ { res, sql: SubQuery alias genState, alias, st: genState }

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
