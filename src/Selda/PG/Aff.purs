module Selda.PG.Aff where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Either (Either)
import Database.PostgreSQL (class FromSQLRow, class ToSQLRow, Connection, PGError)
import Effect.Aff (Aff)
import Heterogeneous.Folding (class HFoldl)
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Selda (Col, FullQuery, Table)
import Selda as S
import Selda.Col (class GetCols)
import Selda.PG.Utils (class ColsToPGHandler, class MkTupleToRecord, class RowListLength, class TableToColsWithoutAlias, RecordToTuple)
import Selda.Table (class TableColumnNames)
import Selda.Table.Constraint (class CanInsertColumnsIntoTable)

runSelda
  ∷ ∀ a
  . Connection
  → ExceptT PGError (ReaderT Connection Aff) a
  → Aff (Either PGError a)
runSelda conn m = runReaderT (runExceptT m) conn

insert_
  ∷ ∀ r t rlcols tup
  . RL.RowToList r rlcols
  ⇒ CanInsertColumnsIntoTable rlcols t
  ⇒ TableColumnNames rlcols
  ⇒ RowListLength rlcols
  ⇒ FromSQLRow tup
  ⇒ ToSQLRow tup
  ⇒ MkTupleToRecord tup r
  ⇒ HFoldl RecordToTuple Unit { | r } tup
  ⇒ Connection → Table t → Array { | r } → Aff (Either PGError Unit)
insert_ conn t r = runSelda conn $ S.insert_ t r

insert
  ∷ ∀ r t rlcols tup
  . RL.RowToList r rlcols
  ⇒ CanInsertColumnsIntoTable rlcols t
  ⇒ TableColumnNames rlcols
  ⇒ RowListLength rlcols
  ⇒ FromSQLRow tup
  ⇒ ToSQLRow tup
  ⇒ MkTupleToRecord tup r
  ⇒ HFoldl RecordToTuple Unit { | r } tup
  ⇒ Connection → Table t → Array { | r } → Aff (Either PGError (Array { | r }))
insert conn t r = runSelda conn $ S.insert t r

query
  ∷ ∀ o i tup s
  . ColsToPGHandler s i tup o
  ⇒ GetCols i
  ⇒ FromSQLRow tup
  ⇒ Connection → FullQuery s (Record i) → Aff (Either PGError (Array { | o }))
query conn q = runSelda conn $ S.query q

deleteFrom
  ∷ ∀ r s r'
  . TableToColsWithoutAlias r r'
  ⇒ Connection 
  → Table r 
  → ({ | r' } → Col s Boolean) 
  → Aff (Either PGError Unit)
deleteFrom conn table pred = runSelda conn $ S.deleteFrom table pred

update
  ∷ ∀ r s r'
  . TableToColsWithoutAlias r r'
  ⇒ GetCols r'
  ⇒ Connection 
  → Table r 
  → ({ | r' } → Col s Boolean) 
  → ({ | r' } → { | r' })
  → Aff (Either PGError Unit)
update conn table pred up = runSelda conn $ S.update table pred up
