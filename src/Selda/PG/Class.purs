module Selda.PG.Class
  ( class MonadSeldaPG
  , class InsertRecordIntoTableReturning
  , insertRecordIntoTableReturning
  , insert_
  , insert
  , insert1
  , insert1_
  , query
  , query1
  , deleteFrom
  , update
  , BackendPGClass
  ) where

import Prelude
import Control.Monad.Reader (ask)
import Data.Array (concat, null)
import Data.Array as Array
import Data.Array.Partial (head)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (Connection, class FromSQLRow, class ToSQLRow, class ToSQLValue, PGError, toSQLValue)
import Database.PostgreSQL as PostgreSQL
import Database.PostgreSQL.PG as PostgreSQL.PG
import Foreign (Foreign)
import Heterogeneous.Folding (class HFoldl, hfoldl)
import Partial.Unsafe (unsafePartial)
import Prim.RowList as RL
import Selda.Col (class GetCols, Col)
import Selda.Expr (ShowM)
import Selda.PG (showInsert1, showPG)
import Selda.Query (limit)
import Selda.Query.Class (class GenericDelete, class GenericInsert, class GenericQuery, class GenericUpdate, class MonadSelda, genericDelete, genericInsert, genericInsert_, genericQuery, genericUpdate)
import Selda.Query.ShowStatement (class GenericShowInsert, showDeleteFrom, showQuery, showUpdate)
import Selda.Query.Type (FullQuery(..), runFullQuery)
import Selda.Query.Utils (class ColsToPGHandler, class RowListLength, class TableToColsWithoutAlias, class ToForeign, RecordToArrayForeign, RecordToTuple(..), colsToPGHandler, tableToColsWithoutAlias)
import Selda.Table (class TableColumnNames, Table)
import Selda.Table.Constraint (class CanInsertColumnsIntoTable)
import Type.Proxy (Proxy(..))

type B = BackendPGClass

data BackendPGClass

class MonadSelda m PGError Connection <= MonadSeldaPG m

instance monadSeldaPGInstance ::
  MonadSelda m PGError Connection =>
  MonadSeldaPG m

pgQuery
  :: forall i o m
   . ToSQLRow i
  => FromSQLRow o
  => MonadSeldaPG m
  => PostgreSQL.Query i o
  -> i
  -> m (Array o)
pgQuery q xTup = do
  conn <- ask
  PostgreSQL.PG.query conn q xTup

pgExecute
  :: forall m
   . MonadSeldaPG m
  => ShowM
  -> m Unit
pgExecute m = when (strQuery /= "") do
  conn <- ask
  PostgreSQL.PG.execute conn (PostgreSQL.Query strQuery) params
  where
  { strQuery, params } = showPG m

-- | Executes an insert query for each input record.
insert_
  :: forall m t r
   . GenericInsert BackendPGClass m t r
  => MonadSeldaPG m
  => Table t
  -> Array { | r }
  -> m Unit
insert_ = genericInsert (Proxy :: Proxy BackendPGClass)

insert1_
  :: forall m t r
   . GenericInsert BackendPGClass m t r
  => MonadSeldaPG m
  => Table t
  -> { | r }
  -> m Unit
insert1_ table r = insert_ table [ r ]

-- | Executes an insert query for each input record.
-- | Column constraints: `Default`s are optional, `Auto`s are forbidden
insert
  :: forall m r t ret
   . InsertRecordIntoTableReturning r t ret
  => MonadSeldaPG m
  => Table t
  -> Array { | r }
  -> m (Array { | ret })
insert table xs = concat <$> traverse ins1 xs
  where
  ins1 r = insertRecordIntoTableReturning r table

insert1
  :: forall m r t ret
   . InsertRecordIntoTableReturning r t ret
  => MonadSeldaPG m
  => Table t
  -> { | r }
  -> m { | ret }
insert1 table r = unsafePartial $ head <$> insertRecordIntoTableReturning r table

-- | Inserts `{ | r }` into `Table t`. Checks constraints (Auto, Default).
-- | Returns inserted record with every column from `Table t`.
class InsertRecordIntoTableReturning r t ret | r t -> ret where
  insertRecordIntoTableReturning
    :: forall m. MonadSeldaPG m => { | r } -> Table t -> m (Array { | ret })

instance insertRecordIntoTableReturningInstance ::
  ( RL.RowToList r rlcols
  , CanInsertColumnsIntoTable rlcols t
  , TableColumnNames rlcols
  , RowListLength rlcols
  , ToSQLRow rTuple
  , FromSQLRow trTuple
  , HFoldl RecordToTuple Unit { | r } rTuple
  , TableToColsWithoutAlias s t tr
  , RL.RowToList tr trl
  , TableColumnNames trl
  , ColsToPGHandler s tr trTuple ret
  ) =>
  InsertRecordIntoTableReturning r t ret where
  insertRecordIntoTableReturning r table = do
    let
      s = (Proxy :: Proxy s)
      colsToinsert = (Proxy :: Proxy rlcols)
      rTuple = hfoldl RecordToTuple unit r

      tr = tableToColsWithoutAlias s table
      colsToRet = (Proxy :: Proxy trl)
      q = showInsert1 table colsToinsert colsToRet
    rows <- pgQuery (PostgreSQL.Query q) rTuple
    pure $ map (colsToPGHandler s tr) rows

instance pgToForeign :: ToSQLValue a => ToForeign BackendPGClass a where
  toForeign _ = toSQLValue

instance genericInsertPGClass ::
  ( HFoldl
      (RecordToArrayForeign BackendPGClass)
      (Array Foreign)
      { | r }
      (Array Foreign)
  , MonadSeldaPG m
  , GenericShowInsert t r
  ) =>
  GenericInsert BackendPGClass m t r where
  genericInsert = genericInsert_ { exec, ph: "$" }
    where
    exec q l =
      when (not $ null l) do
        conn <- ask
        PostgreSQL.PG.execute conn (PostgreSQL.Query q) l

query
  :: forall o i m
   . GenericQuery BackendPGClass m i o
  => FullQuery B { | i }
  -> m (Array { | o })
query = genericQuery (Proxy :: Proxy BackendPGClass)

query1
  :: forall o i m
   . GenericQuery BackendPGClass m i o
  => FullQuery B { | i }
  -> m (Maybe { | o })
query1 (FullQuery q) = query (FullQuery (limit 1 >>= \_ -> q)) <#> Array.head

instance genericQueryPG ::
  ( ColsToPGHandler B i tup o
  , GetCols i
  , FromSQLRow tup
  , MonadSeldaPG m
  ) =>
  GenericQuery BackendPGClass m i o where
  genericQuery _ q = do
    let
      (Tuple res _) = runFullQuery q

      { strQuery, params } = showPG $ showQuery q
    rows <- pgQuery (PostgreSQL.Query strQuery) params
    pure $ map (colsToPGHandler (Proxy :: Proxy BackendPGClass) res) rows

deleteFrom
  :: forall t r m
   . GenericDelete BackendPGClass m t r
  => Table t
  -> ({ | r } -> Col B Boolean)
  -> m Unit
deleteFrom = genericDelete (Proxy :: Proxy BackendPGClass)

instance genericDeletePG ::
  ( TableToColsWithoutAlias B t r
  , MonadSeldaPG m
  ) =>
  GenericDelete BackendPGClass m t r where
  genericDelete _ table pred = pgExecute $ showDeleteFrom table pred

update
  :: forall t r m
   . GenericUpdate BackendPGClass m t r
  => Table t
  -> ({ | r } -> Col B Boolean)
  -> ({ | r } -> { | r })
  -> m Unit
update = genericUpdate (Proxy :: Proxy BackendPGClass)

instance genericUpdatePG ::
  ( TableToColsWithoutAlias B t r
  , GetCols r
  , MonadSeldaPG m
  ) =>
  GenericUpdate BackendPGClass m t r where
  genericUpdate _ table pred up = pgExecute $ showUpdate table pred up
