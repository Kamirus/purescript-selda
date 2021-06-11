module Selda.PG.Aff
  ( insert_
  , insert_'
  -- , insert
  -- , insert1
  , insert1_
  , insert1_'
  , query
  , query1
  , PGSelda
  -- , deleteFrom
  -- , update
  ) where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.Array (head) as Array
import Data.Either (Either)
import Data.Maybe (Maybe)
import Database.PostgreSQL (class FromSQLRow, Connection, PGError, fromSQLRow)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Heterogeneous.Folding (class HFoldl, hfoldl)
import Selda (Col, Table)
import Selda.Col (class GetCols)
import Selda.PG.Class (BackendPGClass)
import Selda.PG.Class as Selda.PG
import Selda.Query (limit)
import Selda.Query.Class (class GenericInsert, runSelda)
import Selda.Query.Type (FullQuery(..))
import Selda.Query.Utils (class ColsToPGHandler, class TableToColsWithoutAlias, RecordToArrayForeign(..))
import Type.Proxy (Proxy(..))

type PGSelda = ExceptT PGError (ReaderT Connection Aff)

type B = BackendPGClass

insert_
  ∷ ∀ t r
  . GenericInsert BackendPGClass PGSelda t r
  ⇒ HFoldl
      (RecordToArrayForeign BackendPGClass)
      (Array Foreign)
      { | r }
      (Array Foreign)
  ⇒ Connection → Table t → Array { | r } → Aff (Either PGError Unit)
insert_ conn t r =
  runSelda conn $ Selda.PG.insert_ t r

insert_'
  ∷ ∀ t r
  . GenericInsert BackendPGClass PGSelda t r
  ⇒ Connection
  → Table t
  → ({ | r } → Array Foreign)
  → Array { | r }
  → Aff (Either PGError Unit)
insert_' conn t decodeRow r =
  runSelda conn $ Selda.PG.insert_' t decodeRow r

insert1_
  ∷ ∀ r t
  . GenericInsert BackendPGClass PGSelda t r
  ⇒ HFoldl
      (RecordToArrayForeign BackendPGClass)
      (Array Foreign)
      { | r }
      (Array Foreign)
  ⇒ Connection
  → Table t
  → { | r }
  → Aff (Either PGError Unit)
insert1_ conn t r = runSelda conn $ Selda.PG.insert1_ t r

insert1_'
  ∷ ∀ r t
  . GenericInsert BackendPGClass PGSelda t r
  ⇒ Connection
  → Table t
  → ({ | r } → Array Foreign)
  → { | r }
  → Aff (Either PGError Unit)
insert1_' conn t encode r = runSelda conn $ Selda.PG.insert1_' t encode r
--
-- insert
--   ∷ ∀ r t tr
--   . InsertRecordIntoTableReturning r t tr
--   ⇒ Connection → Table t → Array { | r } → Aff (Either PGError (Array { | tr }))
-- insert conn t r = runSelda conn $ Selda.PG.insert t r
--
-- insert1
--   ∷ ∀ r t tr
--   . InsertRecordIntoTableReturning r t tr
--   ⇒ Connection → Table t → { | r } → Aff (Either PGError { | tr })
-- insert1 conn t r = runSelda conn $ Selda.PG.insert1 t r

query
  ∷ ∀ o i tup
  . ColsToPGHandler B i tup o
  ⇒ GetCols i
  ⇒ FromSQLRow tup
  ⇒ Connection
  → FullQuery B (Record i)
  → Aff (Either PGError (Array { | o }))
query conn = runSelda conn <<< Selda.PG.query

query'
  ∷ ∀ o i tup
  . GetCols i
  ⇒ Connection
  → FullQuery B { | i }
  → (Array Foreign -> Either String { | o })
  → Aff (Either PGError (Array { | o }))
query' conn q decodeRow = runSelda conn $ Selda.PG.query' q decodeRow

query1
  ∷ ∀ o i tup
  . ColsToPGHandler B i tup o
  ⇒ GetCols i
  ⇒ FromSQLRow tup
  ⇒ Connection
  → FullQuery B (Record i)
  → Aff (Either PGError (Maybe { | o }))
query1 conn (FullQuery q) =
  query conn (FullQuery (limit 1 >>= \_ → q)) <#> map Array.head

query1'
  ∷ ∀ o i tup
  . GetCols i
  ⇒ Connection
  → FullQuery B (Record i)
  → (Array Foreign -> Either String { | o })
  → Aff (Either PGError (Maybe { | o }))
query1' conn (FullQuery q) decodeRow =
  query' conn (FullQuery (limit 1 >>= \_ → q)) decodeRow <#> map Array.head

-- deleteFrom
--   ∷ ∀ r r'
--   . TableToColsWithoutAlias B r r'
--   ⇒ Connection
--   → Table r
--   → ({ | r' } → Col B Boolean)
--   → Aff (Either PGError Unit)
-- deleteFrom conn table pred = runSelda conn $ Selda.PG.deleteFrom table pred
--
-- update
--   ∷ ∀ r r'
--   . TableToColsWithoutAlias B r r'
--   ⇒ GetCols r'
--   ⇒ Connection
--   → Table r
--   → ({ | r' } → Col B Boolean)
--   → ({ | r' } → { | r' })
--   → Aff (Either PGError Unit)
-- update conn table pred up = runSelda conn $ Selda.PG.update table pred up
