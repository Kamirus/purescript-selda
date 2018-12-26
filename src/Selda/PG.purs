module Selda.PG
  ( MonadSelda
  , hoistSelda
  , hoistSeldaWith
  , insert_
  , insert
  , deleteFrom
  , query
  , runSelda
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
import Heterogeneous.Folding (class HFoldl, class HFoldlWithIndex, hfoldl)
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Selda.Col (class GetCols, Col, getCols, showCol)
import Selda.Expr (showExpr)
import Selda.PG.ShowQuery (showState)
import Selda.PG.Utils (class ColsToPGHandler, class TableToColsWithoutAlias, class TupleToRecord, RecordLength(..), RecordToTuple(..), TupleToRecordFunc, colsToPGHandler, tableToColsWithoutAlias, tupleToRecord)
import Selda.Query (class FromTable)
import Selda.Query (selectFrom) as Query
import Selda.Query.Type (FullQuery, Query(..), runQuery)
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

insert_
  ∷ ∀ r rl tup
  . HFoldlWithIndex TupleToRecordFunc (Unit → {}) { | r } (tup → { | r })
  ⇒ FromSQLRow tup
  ⇒ ToSQLRow tup
  ⇒ RL.RowToList r rl
  ⇒ TableColumnNames rl
  ⇒ HFoldl RecordToTuple Unit { | r } tup
  ⇒ HFoldl RecordLength Int { | r } Int
  ⇒ Table r → Array { | r } → MonadSelda Unit
insert_ t r = void $ insert t r

insert
  ∷ ∀ r rl tup
  . TupleToRecord tup r
  ⇒ FromSQLRow tup
  ⇒ ToSQLRow tup
  ⇒ RL.RowToList r rl
  ⇒ TableColumnNames rl
  ⇒ HFoldl RecordToTuple Unit { | r } tup
  ⇒ HFoldl RecordLength Int { | r } Int
  ⇒ Table r → Array { | r } → MonadSelda (Array { | r })
insert (Table { name }) xs = concat <$> traverse insert1 xs
  where
  insert1 x = do
    let
      cols = joinWith ", " $ tableColumnNames (RLProxy ∷ RLProxy rl)
      xTup = hfoldl RecordToTuple unit x
      xLen = hfoldl RecordLength 0 x
      placeholders =
        Array.range 1 xLen # map (\i → "$" <> show i) # joinWith ", "
      qStr =
        "INSERT INTO " <> name <> " (" <> cols <> ") " 
          <> "VALUES " <> "(" <> placeholders <> ") "
          <> "RETURNING " <> cols
    rows ← pgQuery (PostgreSQL.Query qStr) xTup
    pure $ map (tupleToRecord x) rows

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
