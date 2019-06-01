module Selda.PG
  ( MonadSelda
  , hoistSelda
  , hoistSeldaWith
  , insert_
  , insert
  , deleteFrom
  , query
  , runSelda
  , selectFrom
  , update
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader (class MonadReader, ask, runReaderT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Reader.Trans (ReaderT)
import Data.Array (concat)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Exists (runExists)
import Data.Newtype (unwrap)
import Data.String (joinWith)
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
import Selda.Col (class GetCols, Col, getCols, showCol)
import Selda.Expr (showExpr)
import Selda.PG.ShowQuery (showState)
import Selda.PG.Utils (class ColsToPGHandler, class MkTupleToRecord, class RowListLength, class TableToColsWithoutAlias, RecordLength(..), RecordToTuple(..), colsToPGHandler, mkTupleToRecord, rowListLength, tableToColsWithoutAlias)
import Selda.Query (class FromTable)
import Selda.Query (selectFrom) as Query
import Selda.Query.Type (FullQuery, Query, runQuery)
import Selda.Table (class TableColumnNames, Table(..), tableColumnNames)
import Type.Proxy (Proxy(..))
import Type.Row (RLProxy(..))

type MonadSelda a = ExceptT PGError (ReaderT PostgreSQL.Connection Aff) a

runSelda
  ∷ ∀ a
  . PostgreSQL.Connection → MonadSelda a → Aff (Either PGError a)
runSelda conn m = runReaderT (runExceptT m) conn

hoistSelda
  ∷ ∀ m
  . MonadAff m
  ⇒ MonadError PGError m
  ⇒ MonadReader PostgreSQL.Connection m
  ⇒ MonadSelda ~> m
hoistSelda = hoistSeldaWith identity identity

hoistSeldaWith
  ∷ ∀ e m r
  . MonadAff m
  ⇒ MonadError e m
  ⇒ MonadReader r m
  ⇒ (PGError → e) → (r → PostgreSQL.Connection) → MonadSelda ~> m
hoistSeldaWith fe fr m = do
  conn ← asks fr
  r ← liftAff $ runReaderT (runExceptT m) conn
  case r of
    Right a → pure a
    Left pgError → throwError (fe pgError)

pgQuery ∷ ∀ i o. ToSQLRow i ⇒ FromSQLRow o ⇒ PostgreSQL.Query i o → i → MonadSelda (Array o)
pgQuery q xTup = do
  conn ← ask
  PostgreSQL.PG.hoist $ PostgreSQL.PG.query conn q xTup

pgExecute ∷ ∀ i o. ToSQLRow i ⇒ PostgreSQL.Query i o → i → MonadSelda Unit
pgExecute q xTup = do
  conn ← ask
  PostgreSQL.PG.hoist $ PostgreSQL.PG.execute conn q xTup

-- | Executes an insert query for each input record.
insert_
  ∷ ∀ r rl tup
  . RL.RowToList r rl
  ⇒ TableColumnNames rl
  ⇒ RowListLength rl
  ⇒ FromSQLRow tup
  ⇒ ToSQLRow tup
  ⇒ MkTupleToRecord tup r
  ⇒ HFoldl RecordToTuple Unit { | r } tup
  ⇒ Table r → Array { | r } → MonadSelda Unit
insert_ t r = void $ insert t r

-- | Executes an insert query for each input record.
insert
  ∷ ∀ r rl tup
  . RL.RowToList r rl
  ⇒ TableColumnNames rl
  ⇒ RowListLength rl
  ⇒ FromSQLRow tup
  ⇒ ToSQLRow tup
  ⇒ MkTupleToRecord tup r
  ⇒ HFoldl RecordToTuple Unit { | r } tup
  ⇒ Table r → Array { | r } → MonadSelda (Array { | r })
insert table xs = concat <$> traverse insert1 xs
  where
  insert1 ∷ { | r } → MonadSelda (Array { | r })
  insert1 r = do
    let rTup = hfoldl RecordToTuple unit r
    rows ← pgQuery (PostgreSQL.Query (showInsert1 table)) rTup
    pure $ map (mkTupleToRecord r) rows

showInsert1
  ∷ ∀ r rl
  . RL.RowToList r rl
  ⇒ TableColumnNames rl
  ⇒ RowListLength rl
  ⇒ Table r → String
showInsert1 (Table { name }) =
  let
    cols = joinWith ", " $ tableColumnNames (RLProxy ∷ RLProxy rl)
    len = rowListLength (RLProxy ∷ RLProxy rl)
    placeholders =
      Array.range 1 len # map (\i → "$" <> show i) # joinWith ", "
  in
  "INSERT INTO " <> name <> " (" <> cols <> ") " 
    <> "VALUES " <> "(" <> placeholders <> ") "
    <> "RETURNING " <> cols

query
  ∷ ∀ o i tup s
  . ColsToPGHandler s i tup o
  ⇒ GetCols i
  ⇒ FromSQLRow tup
  ⇒ FullQuery s (Record i)
  → MonadSelda (Array (Record o))
query q = do
  let
    (Tuple res st') = runQuery $ unwrap q
    st = st' { cols = getCols res }
    q_str = showState st
  rows ← pgQuery (PostgreSQL.Query q_str) PostgreSQL.Row0
  pure $ map (colsToPGHandler (Proxy ∷ Proxy s) res) rows

selectFrom
  ∷ ∀ cols o i r s tup
  . ColsToPGHandler s i tup o
  ⇒ FromSQLRow tup
  ⇒ FromTable s r cols
  ⇒ GetCols i
  ⇒ Table r
  → ({ | cols } → Query s { | i })
  → MonadSelda (Array (Record o))
selectFrom table q = query (Query.selectFrom table q)

deleteFrom
  ∷  ∀ r s r'
  . TableToColsWithoutAlias r r'
  ⇒ Table r
  → ({ | r' } → Col s Boolean)
  → MonadSelda Unit
deleteFrom table@(Table { name }) pred = do
  let
    recordWithCols = tableToColsWithoutAlias table
    pred_str = showCol $ pred recordWithCols
    q_str = "DELETE FROM " <> name <> " WHERE " <> pred_str
  pgExecute (PostgreSQL.Query q_str) PostgreSQL.Row0

update
  ∷  ∀ r s r'
  . TableToColsWithoutAlias r r'
  ⇒ GetCols r'
  ⇒ Table r
  → ({ | r' } → Col s Boolean)
  → ({ | r' } → { | r' })
  → MonadSelda Unit
update table@(Table { name }) pred up = do
  let
    recordWithCols = tableToColsWithoutAlias table
    pred_str = showCol $ pred recordWithCols
    vals =
      getCols (up recordWithCols)
        # map (\(Tuple n e) → n <> " = " <> runExists showExpr e)
        # joinWith ", "
    q_str = "UPDATE " <> name <> " SET " <> vals <> " WHERE " <> pred_str
  pgExecute (PostgreSQL.Query q_str) PostgreSQL.Row0
