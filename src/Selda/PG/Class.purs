module Selda.PG.Class
  ( class MonadSeldaPG
  , class InsertRecordIntoTableReturning
  , insertRecordIntoTableReturning
  , insert_
  , insert
  , insert1
  , insert1_
  , query
  , deleteFrom
  , update
  , BackendPGClass
  ) where

import Prelude

import Control.Monad.Reader (ask)
import Data.Array (concat)
import Data.Array.Partial (head)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (class FromSQLRow, class ToSQLRow, class ToSQLValue, Connection, PGError, toSQLValue)
import Database.PostgreSQL as PostgreSQL
import Database.PostgreSQL.PG as PostgreSQL.PG
import Foreign (Foreign)
import Heterogeneous.Folding (class HFoldl, hfoldl)
import Partial.Unsafe (unsafePartial)
import Prim.RowList as RL
import Selda.Col (class GetCols, Col)
import Selda.Expr (ShowM)
import Selda.PG (showInsert1, showPG)
import Selda.Query.Class (class GenericInsert, class GenericQuery, class MonadSelda, genericInsert, genericQuery)
import Selda.Query.ShowStatement (genericShowInsert, showDeleteFrom, showQuery, showUpdate)
import Selda.Query.Type (FullQuery, runQuery)
import Selda.Query.Utils (class ColsToPGHandler, class RowListLength, class TableToColsWithoutAlias, class ToForeign, RecordToArrayForeign(..), RecordToTuple(..), colsToPGHandler, tableToColsWithoutAlias)
import Selda.Table (class TableColumnNames, Table)
import Selda.Table.Constraint (class CanInsertColumnsIntoTable)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

data BackendPGClass

class MonadSelda m PGError Connection <= MonadSeldaPG m

instance monadSeldaPGInstance
  ∷ MonadSelda m PGError Connection
  ⇒ MonadSeldaPG m

pgQuery 
  ∷ ∀ i o m
  . ToSQLRow i
  ⇒ FromSQLRow o
  ⇒ MonadSeldaPG m 
  ⇒ PostgreSQL.Query i o → i → m (Array o)
pgQuery q xTup = do
  conn ← ask
  PostgreSQL.PG.query conn q xTup

pgExecute
  ∷ ∀ m
  . MonadSeldaPG m 
  ⇒ ShowM → m Unit
pgExecute m = do
  conn ← ask
  let { strQuery, params } = showPG m
  PostgreSQL.PG.execute conn (PostgreSQL.Query strQuery) params

-- | Executes an insert query for each input record.
insert_
  ∷ ∀ m t r
  . GenericInsert BackendPGClass m t r
  ⇒ MonadSeldaPG m
  ⇒ Table t → Array { | r } → m Unit
insert_ = genericInsert (Proxy ∷ Proxy BackendPGClass)

insert1_
  ∷ ∀ m t r
  . GenericInsert BackendPGClass m t r
  ⇒ MonadSeldaPG m
  ⇒ Table t → { | r } → m Unit
insert1_ table r = insert_ table [r]

-- | Executes an insert query for each input record.
-- | Column constraints: `Default`s are optional, `Auto`s are forbidden
insert
  ∷ ∀ m r t ret
  . InsertRecordIntoTableReturning r t ret
  ⇒ MonadSeldaPG m
  ⇒ Table t → Array { | r } → m (Array { | ret })
insert table xs = concat <$> traverse ins1 xs
  where ins1 r = insertRecordIntoTableReturning r table

insert1
  ∷ ∀ m r t ret
  . InsertRecordIntoTableReturning r t ret
  ⇒ MonadSeldaPG m
  ⇒ Table t → { | r } → m { | ret }
insert1 table r =
  unsafePartial $ head <$> insertRecordIntoTableReturning r table

-- | Inserts `{ | r }` into `Table t`. Checks constraints (Auto, Default).
-- | Returns inserted record with every column from `Table t`.
class InsertRecordIntoTableReturning r t ret | r t → ret where
  insertRecordIntoTableReturning
    ∷ ∀ m. MonadSeldaPG m ⇒ { | r } → Table t → m (Array { | ret })

instance insertRecordIntoTableReturningInstance
    ∷ ( RL.RowToList r rlcols
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
      )
    ⇒ InsertRecordIntoTableReturning r t ret
  where
  insertRecordIntoTableReturning r table = do
    let
      s = (Proxy ∷ Proxy s)
      colsToinsert = (RLProxy ∷ RLProxy rlcols)
      rTuple = hfoldl RecordToTuple unit r
      tr = tableToColsWithoutAlias s table
      colsToRet = (RLProxy ∷ RLProxy trl)
      q = showInsert1 table colsToinsert colsToRet
    rows ← pgQuery (PostgreSQL.Query q) rTuple
    pure $ map (colsToPGHandler s tr) rows

instance pgToForeign ∷ ToSQLValue a ⇒ ToForeign BackendPGClass a where
  toForeign _ = toSQLValue

instance genericInsertPGClass
    ∷ ( HFoldl (RecordToArrayForeign BackendPGClass)
          (Array Foreign) { | r } (Array Foreign)
      , MonadSeldaPG m
      , TableColumnNames rl
      , RL.RowToList r rl
      , CanInsertColumnsIntoTable rl t
      , RowListLength rl
      ) ⇒ GenericInsert BackendPGClass m t r
  where
  genericInsert b table rs = do
    let
      q = genericShowInsert { ph: "$", fstPH: 1 } table rs
      rsTuple = rs >>= hfoldl (RecordToArrayForeign b) ([] ∷ Array Foreign)
    conn ← ask
    PostgreSQL.PG.execute conn (PostgreSQL.Query q) rsTuple

query
  ∷ ∀ o i s m
  . GenericQuery BackendPGClass m s i o
  ⇒ FullQuery s { | i } → m (Array { | o })
query = genericQuery (Proxy ∷ Proxy BackendPGClass)

instance genericQueryPG
    ∷ ( ColsToPGHandler s i tup o
      , GetCols i
      , FromSQLRow tup
      , MonadSeldaPG m
      ) ⇒ GenericQuery BackendPGClass m s i o
  where
  genericQuery _ q = do
    let
      (Tuple res _) = runQuery $ unwrap q
      { strQuery, params } = showPG $ showQuery q
    rows ← pgQuery (PostgreSQL.Query strQuery) params
    pure $ map (colsToPGHandler (Proxy ∷ Proxy s) res) rows

deleteFrom
  ∷ ∀ r s r' m
  . TableToColsWithoutAlias s r r'
  ⇒ MonadSeldaPG m
  ⇒ Table r → ({ | r' } → Col s Boolean) → m Unit
deleteFrom table pred = pgExecute $ showDeleteFrom table pred

update
  ∷ ∀ r s r' m
  . TableToColsWithoutAlias s r r'
  ⇒ GetCols r'
  ⇒ MonadSeldaPG m
  ⇒ Table r → ({ | r' } → Col s Boolean) → ({ | r' } → { | r' }) → m Unit
update table pred up = pgExecute $ showUpdate table pred up
