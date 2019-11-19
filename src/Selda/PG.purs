module Selda.PG
  ( showInsert1
  , showQuery
  , showDeleteFrom
  , showUpdate
  , litF
  , showPG
  ) where

import Prelude

import Data.Array as Array
import Data.Exists (runExists)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (class ToSQLValue, toSQLValue)
import Foreign (Foreign)
import Selda.Col (class GetCols, Col(..), getCols, showCol)
import Selda.Expr (Expr(..), ShowM, showExpr, showM)
import Selda.PG.ShowQuery (showState)
import Selda.PG.Utils (class RowListLength, class TableToColsWithoutAlias, rowListLength, tableToColsWithoutAlias)
import Selda.Query.Type (FullQuery, runQuery)
import Selda.Table (class TableColumnNames, Table(..), tableColumnNames)
import Selda.Table.Constraint (class CanInsertColumnsIntoTable)
import Type.Data.RowList (RLProxy)
import Type.Proxy (Proxy(..))

litF ∷ ∀ s a. ToSQLValue a ⇒ a → Col s a
litF = Col <<< EForeign <<< toSQLValue

showPG
  ∷ ShowM
  → { params ∷ Array Foreign, nextIndex ∷ Int, strQuery ∷ String }
showPG = showM "$" 1

showInsert1
  ∷ ∀ t insRLcols retRLcols
  . CanInsertColumnsIntoTable insRLcols t
  ⇒ TableColumnNames insRLcols
  ⇒ TableColumnNames retRLcols
  ⇒ RowListLength insRLcols
  ⇒ Table t → RLProxy insRLcols → RLProxy retRLcols → String
showInsert1 (Table { name }) colsToinsert colsToRet =
  let
    cols = joinWith ", " $ tableColumnNames colsToinsert
    rets = joinWith ", " $ tableColumnNames colsToRet
    len = rowListLength colsToinsert
    placeholders =
      Array.range 1 len # map (\i → "$" <> show i) # joinWith ", "
  in
  "INSERT INTO " <> name <> " (" <> cols <> ") " 
    <> "VALUES " <> "(" <> placeholders <> ") "
    <> "RETURNING " <> rets

showQuery ∷ ∀ i s. GetCols i ⇒ FullQuery s (Record i) → ShowM
showQuery q = showState st
  where
    (Tuple res st') = runQuery $ unwrap q
    st = st' { cols = getCols res }

showDeleteFrom
  ∷ ∀ r s r'
  . TableToColsWithoutAlias s r r'
  ⇒ Table r → ({ | r' } → Col s Boolean) → ShowM
showDeleteFrom table@(Table { name }) pred = do
  let recordWithCols = tableToColsWithoutAlias (Proxy ∷ Proxy s) table
  pred_str ← showCol $ pred recordWithCols
  pure $ "DELETE FROM " <> name <> " WHERE " <> pred_str

showUpdate
  ∷ ∀ r s r'
  . TableToColsWithoutAlias s r r'
  ⇒ GetCols r'
  ⇒ Table r → ({ | r' } → Col s Boolean) → ({ | r' } → { | r' }) → ShowM
showUpdate table@(Table { name }) pred up = do
  let
    recordWithCols = tableToColsWithoutAlias (Proxy ∷ Proxy s) table
    f (Tuple n e) = do 
      s ← runExists showExpr e
      pure $ n <> " = " <> s 
  pred_str ← showCol $ pred recordWithCols
  vals ← joinWith ", " <$> (traverse f $ getCols $ up recordWithCols)
  pure $ "UPDATE " <> name <> " SET " <> vals <> " WHERE " <> pred_str
