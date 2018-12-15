module Selda.PG
  ( withPG
  , MonadSelda
  , query
  , insert_
  , insert
  , deleteFrom
  , update
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Array (concat)
import Data.Array as Array
import Data.Exists (runExists)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (class FromSQLRow, class ToSQLRow, PoolConfiguration)
import Database.PostgreSQL as PG
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Heterogeneous.Folding (class HFoldl, class HFoldlWithIndex, hfoldl)
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Selda.Col (class GetCols, Col, getCols, showCol)
import Selda.Expr (showExpr)
import Selda.PG.ShowQuery (showState)
import Selda.PG.Utils (class ColsToPGHandler, class TableToColsWithoutAlias, class TupleToRecord, RecordLength(..), RecordToTuple(..), TupleToRecordFunc, colsToPGHandler, tableToColsWithoutAlias, tupleToRecord)
import Selda.Query.Type (IQuery, runQuery)
import Selda.Table (class TableColumnNames, Table(..), tableColumnNames)
import Type.Proxy (Proxy(..))
import Type.Row (RLProxy(..))

type MonadSelda a = ReaderT { pool ∷ PG.Pool } Aff a

withPG ∷ PoolConfiguration → MonadSelda ~> Aff
withPG dbconfig m = do
  pool ← PG.newPool dbconfig
  runReaderT m { pool }

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
      q_str = 
        "INSERT INTO " <> name <> " (" <> cols <> ") " 
          <> "VALUES " <> "(" <> placeholders <> ") "
          <> "RETURNING " <> cols
    -- liftEffect $ log q_str
    -- liftEffect $ log $ show xTup
    { pool } ← ask
    liftAff $ PG.withConnection pool \conn → do
      rows ← PG.query conn (PG.Query q_str) xTup
      pure $ map (tupleToRecord x) rows

query
  ∷ ∀ o i tup s
  . ColsToPGHandler s i tup o
  ⇒ GetCols i
  ⇒ FromSQLRow tup
  ⇒ IQuery s (Record i)
  → MonadSelda (Array (Record o))
query q = do
  let
    (Tuple res st') = runQuery $ unwrap q
    st = st' { cols = getCols res }
    q_str = showState st
  -- liftEffect $ log q_str
  { pool } ← ask
  liftAff $ PG.withConnection pool \conn → do
    rows ← PG.query conn (PG.Query q_str) PG.Row0
    pure $ map (colsToPGHandler (Proxy ∷ Proxy s) res) rows

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
  -- liftEffect $ log q_str
  { pool } ← ask
  liftAff $ PG.withConnection pool \conn → do
    PG.execute conn (PG.Query q_str) PG.Row0

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
  -- liftEffect $ log q_str
  { pool } ← ask
  liftAff $ PG.withConnection pool \conn → do
    PG.execute conn (PG.Query q_str) PG.Row0
