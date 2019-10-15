module Selda.PG.Class
  ( class MonadSelda
  , insert_
  , insert
  , query
  , deleteFrom
  , update
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadReader, ReaderT, ask, asks, runReaderT)
import Data.Array (concat)
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
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Selda.PG (showQuery, showInsert1, showDeleteFrom, showUpdate)
import Selda.Col (class GetCols, Col)
import Selda.PG.Utils (class ColsToPGHandler, class MkTupleToRecord, class RowListLength, class TableToColsWithoutAlias, RecordToTuple(..), colsToPGHandler, mkTupleToRecord)
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
  ∷ ∀ i o m
  . ToSQLRow i
  ⇒ MonadSelda m 
  ⇒ PostgreSQL.Query i o → i → m Unit
pgExecute q xTup = do
  conn ← ask
  PostgreSQL.PG.execute conn q xTup

-- | Executes an insert query for each input record.
insert_
  ∷ ∀ r t rlcols tup m
  . RL.RowToList r rlcols
  ⇒ CanInsertColumnsIntoTable rlcols t
  ⇒ TableColumnNames rlcols
  ⇒ RowListLength rlcols
  ⇒ FromSQLRow tup
  ⇒ ToSQLRow tup
  ⇒ MkTupleToRecord tup r
  ⇒ HFoldl RecordToTuple Unit { | r } tup
  ⇒ MonadSelda m
  ⇒ Table t → Array { | r } → m Unit
insert_ t r = void $ insert t r

-- | Executes an insert query for each input record.
-- | Records to be inserted needs to have columns without constraints,
-- | Default ale optional, Auto must be missing
insert
  ∷ ∀ r t rlcols tup m
  . RL.RowToList r rlcols
  ⇒ CanInsertColumnsIntoTable rlcols t
  ⇒ TableColumnNames rlcols
  ⇒ RowListLength rlcols
  ⇒ FromSQLRow tup
  ⇒ ToSQLRow tup
  ⇒ MkTupleToRecord tup r
  ⇒ HFoldl RecordToTuple Unit { | r } tup
  ⇒ MonadSelda m
  ⇒ Table t → Array { | r } → m (Array { | r })
insert table xs = concat <$> traverse insert1 xs
  where
  insert1 ∷ { | r } → m (Array { | r })
  insert1 r = do
    let rTup = hfoldl RecordToTuple unit r
    let rlCols = (RLProxy ∷ RLProxy rlcols)
    rows ← pgQuery (PostgreSQL.Query (showInsert1 table rlCols)) rTup
    pure $ map (mkTupleToRecord r) rows

query
  ∷ ∀ o i tup s m
  . ColsToPGHandler s i tup o
  ⇒ GetCols i
  ⇒ FromSQLRow tup
  ⇒ MonadSelda m
  ⇒ FullQuery s (Record i) → m (Array (Record o))
query q = do
  let (Tuple res _) = runQuery $ unwrap q
  rows ← pgQuery (PostgreSQL.Query (showQuery q)) PostgreSQL.Row0
  pure $ map (colsToPGHandler (Proxy ∷ Proxy s) res) rows

deleteFrom
  ∷ ∀ r s r' m
  . TableToColsWithoutAlias r r'
  ⇒ MonadSelda m
  ⇒ Table r → ({ | r' } → Col s Boolean) → m Unit
deleteFrom table pred = 
  pgExecute (PostgreSQL.Query (showDeleteFrom table pred)) PostgreSQL.Row0

update
  ∷ ∀ r s r' m
  . TableToColsWithoutAlias r r'
  ⇒ GetCols r'
  ⇒ MonadSelda m
  ⇒ Table r → ({ | r' } → Col s Boolean) → ({ | r' } → { | r' }) → m Unit
update table pred up =
  pgExecute (PostgreSQL.Query (showUpdate table pred up)) PostgreSQL.Row0
