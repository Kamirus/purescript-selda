module Selda.PG
  ( showInsert1
  , showQuery
  , showDeleteFrom
  , showUpdate
  ) where

import Prelude

import Data.Array as Array
import Data.Exists (runExists)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Prim.RowList (kind RowList)
import Selda.Col (class GetCols, Col, getCols, showCol)
import Selda.Expr (showExpr)
import Selda.PG.ShowQuery (showState)
import Selda.PG.Utils (class RowListLength, class TableToColsWithoutAlias, rowListLength, tableToColsWithoutAlias)
import Selda.Query.Type (FullQuery, runQuery)
import Selda.Table (class TableColumnNames, Table(..), tableColumnNames)
import Selda.Table.Constraint (class CanInsertColumnsIntoTable)
import Type.Data.RowList (RLProxy)

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

showQuery ∷ ∀ i s. GetCols i ⇒ FullQuery s (Record i) → String
showQuery q = showState st
  where
    (Tuple res st') = runQuery $ unwrap q
    st = st' { cols = getCols res }

showDeleteFrom
  ∷ ∀ r s r'
  . TableToColsWithoutAlias r r'
  ⇒ Table r → ({ | r' } → Col s Boolean) → String
showDeleteFrom table@(Table { name }) pred = 
  "DELETE FROM " <> name <> " WHERE " <> pred_str
    where
      recordWithCols = tableToColsWithoutAlias table
      pred_str = showCol $ pred recordWithCols

showUpdate
  ∷ ∀ r s r'
  . TableToColsWithoutAlias r r'
  ⇒ GetCols r'
  ⇒ Table r → ({ | r' } → Col s Boolean) → ({ | r' } → { | r' }) → String
showUpdate table@(Table { name }) pred up =
  "UPDATE " <> name <> " SET " <> vals <> " WHERE " <> pred_str
    where
      recordWithCols = tableToColsWithoutAlias table
      pred_str = showCol $ pred recordWithCols
      vals =
        getCols (up recordWithCols)
          # map (\(Tuple n e) → n <> " = " <> runExists showExpr e)
          # joinWith ", "
