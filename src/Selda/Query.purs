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
import Selda.Query.Type (FullQuery(..), GenState(..), Order, QBinOp(..), Query, SQL(..), Source(..), freshId, modify_, runFullQuery)
import Selda.Query.Utils (class ContainsOnlyColTypes)
import Selda.Table (class TableColumns, Alias, Column(..), Table(..), tableColumns)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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

restrict ∷ ∀ s. Col s Boolean → Query s Unit
restrict (Col e) = modify_ \st → st { restricts = e : st.restricts }

-- | `nutNull col` adds to the WHERE clause that col is not null
-- | and returns the coerced column.
notNull ∷ ∀ s a. Col s (Maybe a) → Query s (Col s a)
notNull col@(Col e) = do 
  let
    notNullCol = Col $ EUnOp $ mkExists $ UnExp (IsNotNull identity) e
    fromMaybeCol = (unsafeCoerce ∷ Col s (Maybe a) → Col s a)
  restrict notNullCol
  pure $ fromMaybeCol col

crossJoin ∷ ∀ s r res. FromTable s r res ⇒ Table r → Query s { | res }
crossJoin table = do
  { res, sql } ← fromTable table
  modify_ \st → st { source = CrossJoin st.source sql }
  pure res

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
  modify_ \ st → st { source = LeftJoin st.source sql e }
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
  { res, sql } ← fromSubQuery iq
  let Col e = on res
  modify_ \st → st { source = LeftJoin st.source sql e }
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
    ∷ FullQuery (Inner s) { | inner }
    → Query s { res ∷ { | res } , sql ∷ SQL , alias ∷ Alias , st ∷ GenState }

instance fromSubQueryI 
    ∷ ( ContainsOnlyColTypes inner_rl
      , RL.RowToList inner inner_rl
      , HMapWithIndex OuterCols { | inner } { | res0 }
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
