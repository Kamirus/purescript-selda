module Selda.PG where

import Prelude

import Data.Array as Array
import Data.Maybe (maybe)
import Data.String (joinWith)
import Database.PostgreSQL (class ToSQLValue, toSQLValue)
import Foreign (Foreign)
import Selda.Aggr (class Coerce, unsafeFromCol)
import Selda.Col (Col(..), showCol)
import Selda.Expr (Expr(..), ShowM, showM)
import Selda.Query.Utils (class RowListLength, rowListLength)
import Selda.Table (class TableColumnNames, Table(..), tableColumnNames, tableName)
import Selda.Table.Constraint (class CanInsertColumnsIntoTable)

-- | Lift a value `a` to a column expression using `ToSQLValue a`.
-- | Please note that the value will be passed as a query parameter meaning it
-- | won't appear in the SQL query string as a serialized string, but as a
-- | placeholder with an index corresponding to the array of foreign parameters.
litPG :: forall col s a. ToSQLValue a => Coerce col => a -> col s a
litPG = unsafeFromCol <<< Col <<< EForeign <<< toSQLValue

showPGQuery
  :: ShowM
  -> String
showPGQuery = showPG >>> _.strQuery

showPG
  :: ShowM
  -> { params :: Array Foreign, nextIndex :: Int, strQuery :: String }
showPG = showM "$" 1

showInsert1
  :: forall t insRLcols retRLcols proxy1 proxy2
   . CanInsertColumnsIntoTable insRLcols t
  => TableColumnNames insRLcols
  => TableColumnNames retRLcols
  => RowListLength insRLcols
  => Table t
  -> proxy1 insRLcols
  -> proxy2 retRLcols
  -> String
showInsert1 table colsToinsert colsToRet =
  let
    cols = joinWith ", " $ tableColumnNames colsToinsert
    rets = joinWith ", " $ tableColumnNames colsToRet
    len = rowListLength colsToinsert
    placeholders =
      Array.range 1 len # map (\i -> "$" <> show i) # joinWith ", "
  in
    "INSERT INTO " <> tableName table <> " (" <> cols <> ") "
      <> "VALUES "
      <> "("
      <> placeholders
      <> ") "
      <> "RETURNING "
      <> rets

-- | **PG specific** - The extract function retrieves subfields
-- | such as year or hour from date/time values.
-- | e.g. extract "year" (d ∷ Col s JSDate) 
extract :: forall a s. String -> Col s a -> Col s String
extract field srcCol = Col $ Any do
  s <- showCol srcCol
  pure $ "extract(" <> field <> " from " <> s <> ")"

-- | **PG specific** `generate_series(start, stop)` set returning	function
-- | modeled as a Table-like source. It should be used only for querying.
generateSeries :: Int -> Int -> Table (i :: Int)
generateSeries start stop = Source "gs" \maybeAlias ->
  let
    alias = maybe "" identity maybeAlias
  in
    "generate_series(" <> show start <> ", " <> show stop <> ") " <> alias <> " (i)"
