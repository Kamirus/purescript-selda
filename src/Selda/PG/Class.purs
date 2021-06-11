module Selda.PG.Class
  ( class MonadSeldaPG
  -- , class InsertRecordIntoTableReturning
  -- , insertRecordIntoTableReturning
  , insert_
  , insert_'
  -- , insert
  -- , insert1
  , insert1_
  , insert1_'
  , query
  , query'
  , query1
  , query1'
  -- , deleteFrom
  -- , update
  , BackendPGClass
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask)
import Data.Array (concat, null)
import Data.Array as Array
import Data.Array.Partial (head)
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), fst)
import Database.PostgreSQL (class FromSQLRow, class ToSQLRow, class ToSQLValue, Connection, PGError, fromSQLRow, toSQLValue)
import Database.PostgreSQL as PostgreSQL
import Database.PostgreSQL.PG as PostgreSQL.PG
import Effect.Aff.Class (liftAff)
import Foreign (Foreign)
import Heterogeneous.Folding (class HFoldl, hfoldl)
import Partial.Unsafe (unsafePartial)
import Prim.RowList as RL
import Selda.Col (class GetCols, Col)
import Selda.Expr (ShowM)
import Selda.PG (showInsert1, showPG)
import Selda.Query (limit)
import Selda.Query.Class (class GenericInsert, class GenericQuery, class MonadSelda, genericInsert, genericInsert_, genericQuery)
import Selda.Query.ShowStatement (class GenericShowInsert, showDeleteFrom, showQuery, showUpdate)
import Selda.Query.Type (FullQuery(..), runFullQuery)
import Selda.Query.Utils (class ColsToPGHandler, class RowListLength, class TableToColsWithoutAlias, class ToForeign, RecordToArrayForeign(..), RecordToTuple(..), colsToPGHandler, tableToColsWithoutAlias)
import Selda.Table (class TableColumnNames, Table)
import Selda.Table.Constraint (class CanInsertColumnsIntoTable)
import Type.Proxy (Proxy(..))

type B
  = BackendPGClass

data BackendPGClass

class MonadSelda m PGError Connection <= MonadSeldaPG m

instance monadSeldaPGInstance ∷
  MonadSelda m PGError Connection ⇒
  MonadSeldaPG m

pgQuery ∷
  ∀ i o m.
  ToSQLRow i ⇒
  FromSQLRow o ⇒
  MonadSeldaPG m ⇒
  PostgreSQL.Query i o → i → m (Array o)
pgQuery q xTup = do
  conn ← ask
  PostgreSQL.PG.query conn q xTup

-- pgExecute ∷
--   ∀ m.
--   MonadSeldaPG m ⇒
--   ShowM → m Unit
-- pgExecute m = when (strQuery /= "") do
--   conn ← ask
--   PostgreSQL.PG.execute conn (PostgreSQL.Query strQuery) params
--   where { strQuery, params } = showPG m
--
-- | Executes an insert query for each input record.
insert_ ∷
  ∀ m t r.
  GenericInsert BackendPGClass m t r ⇒
  HFoldl (RecordToArrayForeign BackendPGClass) (Array Foreign) { | r } (Array Foreign) ⇒
  MonadSeldaPG m ⇒
  Table t →
  Array { | r } →
  m Unit
insert_ t r = insert_' t encode r
  where
    encode = hfoldl (RecordToArrayForeign _b) ([] ∷ Array Foreign)
    _b = Proxy :: Proxy BackendPGClass

insert_' ∷
  ∀ m t r.
  GenericInsert BackendPGClass m t r ⇒
  MonadSeldaPG m ⇒
  Table t →
  ({ | r } -> Array Foreign) →
  Array { | r } →
  m Unit
insert_' = genericInsert (Proxy ∷ Proxy BackendPGClass)

insert1_ ∷
  ∀ m t r.
  GenericInsert BackendPGClass m t r ⇒
  HFoldl (RecordToArrayForeign BackendPGClass) (Array Foreign) { | r } (Array Foreign) ⇒
  MonadSeldaPG m ⇒
  Table t →
  { | r } →
  m Unit
insert1_ table r = insert1_' table encode r
  where
    encode = hfoldl (RecordToArrayForeign _b) ([] ∷ Array Foreign)
    _b = Proxy :: Proxy BackendPGClass

insert1_' ∷
  ∀ m t r.
  GenericInsert BackendPGClass m t r ⇒
  MonadSeldaPG m ⇒
  Table t →
  ({ | r } -> Array Foreign) →
  { | r } →
  m Unit
insert1_' table encode r = insert_' table encode [ r ]
--
-- -- | Executes an insert query for each input record.
-- -- | Column constraints: `Default`s are optional, `Auto`s are forbidden
-- insert ∷
--   ∀ m r t ret.
--   InsertRecordIntoTableReturning r t ret ⇒
--   MonadSeldaPG m ⇒
--   Table t → Array { | r } → m (Array { | ret })
-- insert table xs = concat <$> traverse ins1 xs
--   where
--   ins1 r = insertRecordIntoTableReturning r table
--
-- insert1 ∷
--   ∀ m r t ret.
--   InsertRecordIntoTableReturning r t ret ⇒
--   MonadSeldaPG m ⇒
--   Table t → { | r } → m { | ret }
-- insert1 table r = unsafePartial $ head <$> insertRecordIntoTableReturning r table
--
-- -- | Inserts `{ | r }` into `Table t`. Checks constraints (Auto, Default).
-- -- | Returns inserted record with every column from `Table t`.
-- class InsertRecordIntoTableReturning r t ret | r t → ret where
--   insertRecordIntoTableReturning ∷
--     ∀ m. MonadSeldaPG m ⇒ { | r } → Table t → m (Array { | ret })
--
-- instance insertRecordIntoTableReturningInstance ∷
--   ( RL.RowToList r rlcols
--   , CanInsertColumnsIntoTable rlcols t
--   , TableColumnNames rlcols
--   , RowListLength rlcols
--   , ToSQLRow rTuple
--   , FromSQLRow trTuple
--   , HFoldl RecordToTuple Unit { | r } rTuple
--   , TableToColsWithoutAlias s t tr
--   , RL.RowToList tr trl
--   , TableColumnNames trl
--   , ColsToPGHandler s tr trTuple ret
--   ) ⇒
--   InsertRecordIntoTableReturning r t ret where
--   insertRecordIntoTableReturning r table = do
--     let
--       s = (Proxy ∷ Proxy s)
--       colsToinsert = (Proxy ∷ Proxy rlcols)
--       rTuple = hfoldl RecordToTuple unit r
--
--       tr = tableToColsWithoutAlias s table
--       colsToRet = (Proxy ∷ Proxy trl)
--       q = showInsert1 table colsToinsert colsToRet
--     rows ← pgQuery (PostgreSQL.Query q) rTuple
--     pure $ map (colsToPGHandler s tr) rows

instance pgToForeign ∷ ToSQLValue a ⇒ ToForeign BackendPGClass a where
  toForeign _ = toSQLValue

instance genericInsertPGClass ∷
  ( MonadSeldaPG m
  , GenericShowInsert t r
  ) ⇒
  GenericInsert BackendPGClass m t r where
  genericInsert b table encode rs =
    genericInsert_ { exec, ph: "$" } b table encode rs
    where
    exec q values =
      when (not $ null values) do
        conn ← ask
        liftAff $ void $ PostgreSQL.unsafeQuery conn q values

query ∷
  ∀ o i m tup.
  ColsToPGHandler B i tup o ⇒
  GetCols i ⇒
  FromSQLRow tup ⇒
  GenericQuery BackendPGClass m i o ⇒
  FullQuery B { | i } →
  m (Array { | o })
query q = do
  let
    res = fst $ runFullQuery q
    decodeRow = colsToPGHandler (Proxy ∷ Proxy BackendPGClass) res
  query' q (map decodeRow <<< fromSQLRow)

query' ∷
  ∀ o i m.
  GenericQuery BackendPGClass m i o ⇒
  FullQuery B { | i } →
  (Array Foreign -> Either String { | o }) →
  m (Array { | o })
query' = genericQuery (Proxy ∷ Proxy BackendPGClass)

query1 ∷
  ∀ o i m tup.
  ColsToPGHandler B i tup o ⇒
  GetCols i ⇒
  FromSQLRow tup ⇒
  GenericQuery BackendPGClass m i o ⇒
  FullQuery B { | i } →
  m (Maybe { | o })
query1 (FullQuery q) =
  query (FullQuery (limit 1 >>= \_ → q)) <#> Array.head

query1' ∷
  ∀ o i m.
  GenericQuery BackendPGClass m i o ⇒
  FullQuery B { | i } →
  (Array Foreign -> Either String { | o }) →
  m (Maybe { | o })
query1' (FullQuery q) decodeRow =
  query' (FullQuery (limit 1 >>= \_ → q)) decodeRow <#> Array.head

instance genericQueryPG ∷
  ( GetCols i
  , MonadSeldaPG m
  ) ⇒
  GenericQuery BackendPGClass m i o where
  genericQuery _ q decodeRow = do
    let { strQuery, params } = showPG $ showQuery q
    conn ← ask
    errOrResult ← liftAff $ PostgreSQL.unsafeQuery conn strQuery params
    either throwError pure $
      errOrResult >>= (_.rows) >>>
        traverse (decodeRow >>> lmap PostgreSQL.ConversionError)

-- deleteFrom ∷
--   ∀ t r m.
--   GenericDelete BackendPGClass m t r ⇒
--   Table t → ({ | r } → Col B Boolean) → m Unit
-- deleteFrom = genericDelete (Proxy ∷ Proxy BackendPGClass)
--
-- instance genericDeletePG ∷
--   ( TableToColsWithoutAlias B t r
--   , MonadSeldaPG m
--   ) ⇒
--   GenericDelete BackendPGClass m t r where
--   genericDelete _ table pred = pgExecute $ showDeleteFrom table pred
--
-- update ∷
--   ∀ t r m.
--   GenericUpdate BackendPGClass m t r ⇒
--   Table t → ({ | r } → Col B Boolean) → ({ | r } → { | r }) → m Unit
-- update = genericUpdate (Proxy ∷ Proxy BackendPGClass)
--
-- instance genericUpdatePG ∷
--   ( TableToColsWithoutAlias B t r
--   , GetCols r
--   , MonadSeldaPG m
--   ) ⇒
--   GenericUpdate BackendPGClass m t r where
--   genericUpdate _ table pred up = pgExecute $ showUpdate table pred up
