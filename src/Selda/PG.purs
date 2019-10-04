module Selda.PG
  ( class MonadSelda
  , insert_
  , insert
  -- , showInsert1
  , query
  -- , showQuery
  , deleteFrom
  -- , showDeleteFrom
  , update
  -- , showUpdate
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadReader, ask)
import Data.Array (concat)
import Data.Array as Array
import Data.Exists (runExists)
import Data.Functor.Variant (VariantF, match)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (class FromSQLRow, class ToSQLRow, PGError)
import Database.PostgreSQL as PostgreSQL
import Database.PostgreSQL.PG as PostgreSQL.PG
import Effect.Aff.Class (class MonadAff)
import Heterogeneous.Folding (class HFoldl, hfoldl)
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Selda.Col (class GetCols, Col(..), ExistsExpr, getCols)
import Selda.Expr (Expr, showExpr)
import Selda.PG.ShowQuery (showState)
import Selda.PG.Utils (class ColsToPGHandler, class MkTupleToRecord, class RowListLength, class TableToColsWithoutAlias, RecordToTuple(..), colsToPGHandler, mkTupleToRecord, rowListLength, tableToColsWithoutAlias)
import Selda.Query (class FromTable)
import Selda.Query (selectFrom) as Query
import Selda.Query.Type (FullQuery, Query, runQuery)
import Selda.Table (class TableColumnNames, Table(..), tableColumnNames)
import Selda.Table.Constraint (class CanInsertColumnsIntoTable)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

class 
  ( MonadAff m
  , MonadError PGError m
  , MonadReader
      { conn ∷ PostgreSQL.Connection
      , showV ∷ ExistsExpr v → String
      }
      m
  ) <= MonadSelda m v

instance monadSeldaInstance
  ∷ ( MonadAff m
    , MonadError PGError m
    , MonadReader
        { conn ∷ PostgreSQL.Connection
        , showV ∷ ExistsExpr v → String
        }
        m
    )
  ⇒ MonadSelda m v

pgQuery 
  ∷ ∀ i o m v
  . ToSQLRow i
  ⇒ FromSQLRow o
  ⇒ MonadSelda m v
  ⇒ PostgreSQL.Query i o → i → m (Array o)
pgQuery q xTup = do
  { conn } ← ask
  PostgreSQL.PG.query conn q xTup

pgExecute
  ∷ ∀ i o m
  . ToSQLRow i
  ⇒ MonadSelda m
  ⇒ PostgreSQL.Query i o → i → m Unit
pgExecute q xTup = do
  { conn } ← ask
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

showInsert1
  ∷ ∀ t rlcols
  . CanInsertColumnsIntoTable rlcols t
  ⇒ TableColumnNames rlcols
  ⇒ RowListLength rlcols
  ⇒ Table t → RLProxy rlcols → String
showInsert1 (Table { name }) colsToinsert =
  let
    cols = joinWith ", " $ tableColumnNames colsToinsert
    len = rowListLength colsToinsert
    placeholders =
      Array.range 1 len # map (\i → "$" <> show i) # joinWith ", "
  in
  "INSERT INTO " <> name <> " (" <> cols <> ") " 
    <> "VALUES " <> "(" <> placeholders <> ") "
    <> "RETURNING " <> cols

query
  ∷ ∀ o i tup s m v
  . ColsToPGHandler s i tup o
  ⇒ GetCols i v
  ⇒ FromSQLRow tup
  ⇒ MonadSelda m v
  ⇒ FullQuery s v (Record i) → m (Array (Record o))
query q = do
  { showV } ← ask
  let (Tuple res _) = runQuery $ unwrap q
  rows ← pgQuery (PostgreSQL.Query (showQuery q)) PostgreSQL.Row0
  pure $ map (colsToPGHandler (Proxy ∷ Proxy s) res) rows
    where
    -- showQuery ∷ ∀ i s v. GetCols i v ⇒ FullQuery s v (Record i) → String
    showQuery q = showState showV st
      where
        (Tuple res st') = runQuery $ unwrap q
        st = st' { cols = getCols res (RProxy ∷ RProxy v) }

deleteFrom
  ∷  ∀ r s r' m v
  . TableToColsWithoutAlias r r'
  ⇒ MonadSelda m
  ⇒ (∀ a. VariantF v a → String)
  → Table r → ({ | r' } → Col s v Boolean) → m Unit
deleteFrom showV table pred =
  pgExecute (PostgreSQL.Query (showDeleteFrom table pred)) PostgreSQL.Row0
    where
  -- showDeleteFrom
  --   ∷  ∀ r s r'
  --   . TableToColsWithoutAlias r r'
  --   ⇒ Table r → ({ | r' } → Col s Boolean) → String
    showDeleteFrom table@(Table { name }) pred = 
      "DELETE FROM " <> name <> " WHERE " <> pred_str
        where
          recordWithCols = tableToColsWithoutAlias table
          pred_str = showV $ unwrap $ pred recordWithCols

update
  ∷  ∀ r s r' m v
  . TableToColsWithoutAlias r r'
  ⇒ GetCols r' v
  ⇒ MonadSelda m
  ⇒ (∀ a. VariantF v a → String)
  → Table r → ({ | r' } → Col s v Boolean) → ({ | r' } → { | r' }) → m Unit
update showV table@(Table { name }) pred up = do
  pgExecute (PostgreSQL.Query (showUpdate showV table pred up)) PostgreSQL.Row0

showUpdate
  ∷  ∀ r s r' v
  . TableToColsWithoutAlias r r'
  ⇒ GetCols r' v
  ⇒ (∀ a. VariantF v a → String)
  → Table r → ({ | r' } → Col s v Boolean) → ({ | r' } → { | r' }) → String
showUpdate showV table@(Table { name }) pred up =
  "UPDATE " <> name <> " SET " <> vals <> " WHERE " <> pred_str
    where
      recordWithCols = tableToColsWithoutAlias table
      pred_str = showV $ unwrap $ pred recordWithCols
      vals =
        getCols (up recordWithCols) (RProxy ∷ RProxy v)
          # map (\(Tuple n e) → n <> " = " <> runExists showV e)
          # joinWith ", "
