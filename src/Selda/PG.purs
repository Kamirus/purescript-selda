module Selda.PG where

import Prelude

import Data.Array as Array
import Data.String (joinWith)
import Database.PostgreSQL (class ToSQLValue, toSQLValue)
import Foreign (Foreign)
import Selda.Col (Col(..))
import Selda.Expr (Expr(..), ShowM, showM)
import Selda.Table (class TableColumnNames, Table(..), tableColumnNames)
import Selda.Table.Constraint (class CanInsertColumnsIntoTable)
import Selda.Query.Utils (class RowListLength, rowListLength)
import Type.Data.RowList (RLProxy)

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
