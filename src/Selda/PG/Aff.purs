module Selda.PG.Aff
  ( insert_
  , insert
  , insert1
  , insert1_
  , query
  , deleteFrom
  , update
  ) where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Either (Either)
import Database.PostgreSQL (class FromSQLRow, Connection, PGError)
import Effect.Aff (Aff)
import Prim.RowList (kind RowList)
import Selda (Col, FullQuery, Table)
import Selda.Col (class GetCols)
import Selda.PG.Class (class InsertRecordIntoTableReturning)
import Selda.PG.Class as S
import Selda.PG.Utils (class ColsToPGHandler, class TableToColsWithoutAlias)

runSelda
  ∷ ∀ a
  . Connection
  → ExceptT PGError (ReaderT Connection Aff) a
  → Aff (Either PGError a)
runSelda conn m = runReaderT (runExceptT m) conn

insert_
  ∷ ∀ r t tr
  . InsertRecordIntoTableReturning r t tr
  ⇒ Connection → Table t → Array { | r } → Aff (Either PGError Unit)
insert_ conn t r = runSelda conn $ S.insert_ t r

insert
  ∷ ∀ r t tr
  . InsertRecordIntoTableReturning r t tr
  ⇒ Connection → Table t → Array { | r } → Aff (Either PGError (Array { | tr }))
insert conn t r = runSelda conn $ S.insert t r

insert1
  ∷ ∀ r t tr
  . InsertRecordIntoTableReturning r t tr
  ⇒ Connection → Table t → { | r } → Aff (Either PGError { | tr })
insert1 conn t r = runSelda conn $ S.insert1 t r

insert1_
  ∷ ∀ r t tr
  . InsertRecordIntoTableReturning r t tr
  ⇒ Connection → Table t → { | r } → Aff (Either PGError Unit)
insert1_ conn t r = runSelda conn $ S.insert1_ t r

query
  ∷ ∀ o i tup s
  . ColsToPGHandler s i tup o
  ⇒ GetCols i
  ⇒ FromSQLRow tup
  ⇒ Connection → FullQuery s (Record i) → Aff (Either PGError (Array { | o }))
query conn q = runSelda conn $ S.query q

deleteFrom
  ∷ ∀ r s r'
  . TableToColsWithoutAlias s r r'
  ⇒ Connection 
  → Table r 
  → ({ | r' } → Col s Boolean) 
  → Aff (Either PGError Unit)
deleteFrom conn table pred = runSelda conn $ S.deleteFrom table pred

update
  ∷ ∀ r s r'
  . TableToColsWithoutAlias s r r'
  ⇒ GetCols r'
  ⇒ Connection 
  → Table r 
  → ({ | r' } → Col s Boolean) 
  → ({ | r' } → { | r' })
  → Aff (Either PGError Unit)
update conn table pred up = runSelda conn $ S.update table pred up
