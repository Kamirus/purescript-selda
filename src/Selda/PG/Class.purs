module Selda.PG.Class
  ( class MonadSelda
  , class InsertRecordIntoTableReturning
  , insertRecordIntoTableReturning
  , insert_
  , insert
  , insert1
  , insert1_
  , query
  , deleteFrom
  , update
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadReader, ReaderT, ask, asks, runReaderT)
import Data.Array (concat)
import Data.Array.Partial (head)
import Data.Either (either)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (class FromSQLRow, class ToSQLRow, PGError)
import Database.PostgreSQL as PostgreSQL
import Database.PostgreSQL.PG as PostgreSQL.PG
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Heterogeneous.Folding (class HFoldl, hfoldl)
import Partial.Unsafe (unsafePartial)
import Prim.RowList as RL
import Selda.Col (class GetCols, Col)
import Selda.Expr (ShowM, showM)
import Selda.PG (showQuery, showInsert1, showDeleteFrom, showUpdate)
import Selda.PG.Utils (class ColsToPGHandler, class RowListLength, class TableToColsWithoutAlias, RecordToTuple(..), colsToPGHandler, tableToColsWithoutAlias)
import Selda.Query.Type (FullQuery, runQuery)
import Selda.Table (class TableColumnNames, Table)
import Selda.Table.Constraint (class CanInsertColumnsIntoTable)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

hoistSeldaWith
  ∷ ∀ e m r
  . MonadAff m
  ⇒ MonadError e m
  ⇒ MonadReader r m
  ⇒ (PGError → e)
  → (r → PostgreSQL.Connection)
  → ExceptT PGError (ReaderT PostgreSQL.Connection Aff) ~> m
hoistSeldaWith fe fr m = do
  conn ← asks fr
  runReaderT (runExceptT m) conn # liftAff
    >>= either (throwError <<< fe) pure

class 
  ( MonadAff m
  , MonadError PGError m
  , MonadReader PostgreSQL.Connection m
  ) <= MonadSelda m

instance monadSeldaInstance
  ∷ ( MonadAff m
    , MonadError PGError m
    , MonadReader PostgreSQL.Connection m
    )
  ⇒ MonadSelda m

pgQuery 
  ∷ ∀ i o m
  . ToSQLRow i
  ⇒ FromSQLRow o
  ⇒ MonadSelda m 
  ⇒ PostgreSQL.Query i o → i → m (Array o)
pgQuery q xTup = do
  conn ← ask
  PostgreSQL.PG.query conn q xTup

pgExecute
  ∷ ∀ m
  . MonadSelda m 
  ⇒ ShowM → m Unit
pgExecute m = do
  conn ← ask
  let { strQuery, params } = showM 1 m
  PostgreSQL.PG.execute conn (PostgreSQL.Query strQuery) params

-- | Executes an insert query for each input record.
insert_
  ∷ ∀ m r t ret
  . InsertRecordIntoTableReturning r t ret
  ⇒ MonadSelda m
  ⇒ Table t → Array { | r } → m Unit
insert_ t r = void $ insert t r

-- | Executes an insert query for each input record.
-- | Column constraints: `Default`s are optional, `Auto`s are forbidden
insert
  ∷ ∀ m r t ret
  . InsertRecordIntoTableReturning r t ret
  ⇒ MonadSelda m
  ⇒ Table t → Array { | r } → m (Array { | ret })
insert table xs = concat <$> traverse ins1 xs
  where ins1 r = insertRecordIntoTableReturning r table

insert1
  ∷ ∀ m r t ret
  . InsertRecordIntoTableReturning r t ret
  ⇒ MonadSelda m
  ⇒ Table t → { | r } → m { | ret }
insert1 table r =
  unsafePartial $ head <$> insertRecordIntoTableReturning r table

insert1_
  ∷ ∀ m r t ret
  . InsertRecordIntoTableReturning r t ret
  ⇒ MonadSelda m
  ⇒ Table t → { | r } → m Unit
insert1_ table r = void $ insert1 table r

-- | Inserts `{ | r }` into `Table t`. Checks constraints (Auto, Default).
-- | Returns inserted record with every column from `Table t`.
class InsertRecordIntoTableReturning r t ret | r t → ret where
  insertRecordIntoTableReturning
    ∷ ∀ m. MonadSelda m ⇒ { | r } → Table t → m (Array { | ret })

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

query
  ∷ ∀ o i tup s m
  . ColsToPGHandler s i tup o
  ⇒ GetCols i
  ⇒ FromSQLRow tup
  ⇒ MonadSelda m
  ⇒ FullQuery s (Record i) → m (Array (Record o))
query q = do
  let
    (Tuple res _) = runQuery $ unwrap q
    { strQuery, params } = showM 1 $ showQuery q
  rows ← pgQuery (PostgreSQL.Query strQuery) params
  pure $ map (colsToPGHandler (Proxy ∷ Proxy s) res) rows

deleteFrom
  ∷ ∀ r s r' m
  . TableToColsWithoutAlias s r r'
  ⇒ MonadSelda m
  ⇒ Table r → ({ | r' } → Col s Boolean) → m Unit
deleteFrom table pred = pgExecute $ showDeleteFrom table pred

update
  ∷ ∀ r s r' m
  . TableToColsWithoutAlias s r r'
  ⇒ GetCols r'
  ⇒ MonadSelda m
  ⇒ Table r → ({ | r' } → Col s Boolean) → ({ | r' } → { | r' }) → m Unit
update table pred up = pgExecute $ showUpdate table pred up
