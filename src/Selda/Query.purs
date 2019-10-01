module Selda.Query where

import Prelude

import Control.Monad.State (modify_)
import Data.Array ((:))
import Data.Exists (mkExists)
import Data.Functor.Variant (FProxy, inj, match)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple(..), snd)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class Mapping, class MappingWithIndex, hmap, hmapWithIndex)
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Selda.Aggr (Aggr(..), UnAggr(..), WrapWithAggr(..))
import Selda.Col (class GetCols, class ToCols, Col(..), ExistsExpr, _expr, getCols, toCols)
import Selda.Expr (Expr(..), UnExp(..), UnOp(..))
import Selda.Inner (Inner, OuterCols(..))
import Selda.Query.Type (FullQuery(..), Order, Query(..), SQL(..), Source(..), freshId, runQuery)
import Selda.Table (class TableColumns, Alias, Column(..), Table(..), tableColumns)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

selectFrom
  ∷ ∀ r s v cols res
  . FromTable s r cols
  ⇒ Table r
  → ({ | cols } → Query s v { | res })
  → FullQuery s v { | res }
selectFrom table k = FullQuery $ crossJoin table >>= k

selectFrom_
  ∷ ∀ inner s v resi reso
  . FromSubQuery s v inner resi
  ⇒ FullQuery (Inner s) v { | inner }
  → ({ | resi } → Query s v { | reso })
  → FullQuery s v { | reso }
selectFrom_ iq k = FullQuery $ crossJoin_ iq >>= k

restrict ∷ ∀ s v. Col s v Boolean → Query s v Unit
restrict (Col e) = Query $ modify_ \st → st { restricts = e : st.restricts }

notNull
  ∷ ∀ s v a
  . Col s ( expr ∷ FProxy Expr ) (Maybe a)
  → Query s ( expr ∷ FProxy Expr | v ) (Col s ( expr ∷ FProxy Expr ) a)
notNull col@(Col v) = do 
  let
    e = match { expr: \(expr ∷ Expr (Maybe a)) → expr } v
    notNullCol = Col $ inj _expr $ EUnOp $ mkExists $ UnExp (IsNull identity) e
    fromMaybeCol = (unsafeCoerce ∷ Col s _ (Maybe a) → Col s _ a)
  restrict notNullCol
  pure $ fromMaybeCol col

crossJoin
  ∷ ∀ s v r res
  . FromTable s r res
  ⇒ Table r → Query s v { | res }
crossJoin table = do
  { res, sql } ← fromTable table
  Query $ modify_ $ \st → st { sources = Product sql : st.sources }
  pure res

crossJoin_
  ∷ ∀ inner s v res
  . FromSubQuery s v inner res
  ⇒ FullQuery (Inner s) v { | inner }
  → Query s v { | res }
crossJoin_ iq = do
  let q = unwrap iq
  { res, sql } ← fromSubQuery q
  Query $ modify_ $ \st → st { sources = Product sql : st.sources }
  pure res

aggregate
  ∷ ∀ s v aggr res
  . HMapWithIndex UnAggr { | aggr } { | res }
  ⇒ FullQuery s v { | aggr }
  → FullQuery s v { | res }
aggregate q = map (hmapWithIndex UnAggr) q

groupBy ∷ ∀ s v a. Col s v a → Query s v (Aggr s v a)
groupBy col@(Col e) = do
  Query $ modify_ \st → st { aggr = st.aggr <> [mkExists e] }
  pure $ Aggr col

groupBy'
  ∷ ∀ i o s v
  . GetCols i v
  ⇒ HMap WrapWithAggr { | i } { | o }
  ⇒ { | i }
  → Query s v { | o }
groupBy' i = do
  let aggr = map snd $ getCols i (RProxy ∷ RProxy v)
  Query $ modify_ \st → st { aggr = st.aggr <> aggr }
  pure $ hmap WrapWithAggr i

orderBy ∷ ∀ s v a. Order → Col s v a → Query s v Unit
orderBy order (Col e) =
  Query $ modify_ \st → st { order = st.order <> [Tuple order $ mkExists e] }

limit ∷ ∀ s v. Int → Query s v Unit
limit i = Query $ modify_ $ _ { limit = Just i }

leftJoin
  ∷ ∀ r s v res mres
  . FromTable s r res
  ⇒ HMap WrapWithMaybe { | res } { | mres }
  ⇒ Table r
  → ({ | res } → Col s v Boolean)
  → Query s v { | mres }
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
  ∷ ∀ s v res mres inner
  . FromSubQuery s v inner res
  ⇒ HMap WrapWithMaybe { | res } { | mres }
  ⇒ ({ | res } → Col s v Boolean)
  → FullQuery (Inner s) v { | inner }
  → Query s v { | mres }
leftJoin_ on iq = do
  let q = unwrap iq
  { res, sql } ← fromSubQuery q
  let Col e = on res
  Query $ modify_ \st → st { sources = LeftJoin sql e : st.sources }
  pure $ hmap WrapWithMaybe res

class FromTable s t c | s t → c where
  fromTable
    ∷ ∀ v
    . Table t
    → Query s v
        { res ∷ { | c }
        , sql ∷ SQL v
        }

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
    ∷ Mapping WrapWithMaybe (Col s v (Maybe a)) (Col s v (Maybe a))
  where
  mapping _ = identity
else instance wrapWithMaybeInstance
    ∷ Mapping WrapWithMaybe (Col s v a) (Col s v (Maybe a))
  where
  mapping _ = (unsafeCoerce ∷ Col s v a → Col s v (Maybe a))

subQueryAlias ∷ ∀ s v. Query s v Alias
subQueryAlias = do
  id ← freshId
  pure $ "sub_q" <> show id

class FromSubQuery s v inner res | s inner → res where
  fromSubQuery
    ∷ Query (Inner s) v { | inner }
    → Query s v
        { res ∷ { | res }
        , sql ∷ SQL v
        , alias ∷ Alias
        }

type Cols v = Array (Tuple String (ExistsExpr v))

instance fromSubQueryI 
    ∷ ( HMap OuterCols { | inner } { | res0 }
      , GetCols res0 v
      , HMapWithIndex SubQueryResult { | res0 } { | res }
      )
    ⇒ FromSubQuery s v inner res
  where
  fromSubQuery q = do
    let (Tuple innerRes st) = runQuery q
    let res0 = hmap OuterCols innerRes
    alias ← subQueryAlias
    let res = createSubQueryResult alias res0
    let cols = getCols res0 (RProxy ∷ RProxy v)
    pure $ { res, sql: SubQuery alias $ st { cols = cols }, alias }

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
    ⇒ MappingWithIndex SubQueryResult (SProxy sym)
        (Col s v a)
        (Col s ( expr ∷ FProxy Expr ) a)
  where
  mappingWithIndex (SubQueryResult namespace) sym (Col _) = 
    Col $ inj _expr $ EColumn $ Column { namespace, name: reflectSymbol sym }
