-- | Common to-string functions for SQL statements
-- | (SELECT, UPDATE, DELETE, INSERT) shared between backends.
module Selda.Query.ShowStatement where

import Prelude

import Data.Array (catMaybes)
import Data.Array as Array
import Data.Exists (runExists)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Prim.RowList as RL
import Selda.Col (class GetCols, Col, getCols, showCol)
import Selda.Expr (ShowM, showExpr)
import Selda.Query.PrettyPrint (PrettyM, dodoPrint, ppState)
import Selda.Query.Type (FullQuery, GenState(..), runFullQuery)
import Selda.Query.Utils (class RowListLength, class TableToColsWithoutAlias, rowListLength, tableToColsWithoutAlias)
import Selda.Table (class TableColumnNames, Table, showColumnName, tableColumnNames, tableName)
import Selda.Table.Constraint (class CanInsertColumnsIntoTable)
import Type.Proxy (Proxy(..))

ppQuery :: forall i s. GetCols i => FullQuery s { | i } -> PrettyM
ppQuery q = ppState st
  where
  (Tuple res (GenState st')) = runFullQuery q
  st = st' { cols = getCols res }

showQuery :: forall i s. GetCols i => FullQuery s (Record i) -> ShowM
showQuery q = dodoPrint <$> ppQuery q

showDeleteFrom
  :: forall t s r
   . TableToColsWithoutAlias s t r
  => Table t
  -> ({ | r } -> Col s Boolean)
  -> ShowM
showDeleteFrom table pred = do
  let recordWithCols = tableToColsWithoutAlias (Proxy :: Proxy s) table
  pred_str <- showCol $ pred recordWithCols
  pure $ "DELETE FROM " <> tableName table <> " WHERE " <> pred_str

showUpdate
  :: forall t s r
   . TableToColsWithoutAlias s t r
  => GetCols r
  => Table t
  -> ({ | r } -> Col s Boolean)
  -> ({ | r } -> { | r })
  -> ShowM
showUpdate table pred up = do
  let
    recordWithCols = tableToColsWithoutAlias (Proxy :: Proxy s) table
    f (Tuple name expr) = do
      updatedValue <- runExists showExpr expr
      let columnName = showColumnName name
      pure $
        if columnName == updatedValue then Nothing
        else Just $ columnName <> " = " <> updatedValue
  pred_str <- showCol $ pred recordWithCols
  vals <- joinWith ", " <$> catMaybes <$> (traverse f $ getCols $ up recordWithCols)
  pure $
    if vals == "" then ""
    else
      "UPDATE " <> tableName table <> " SET " <> vals <> " WHERE " <> pred_str

-- | typeclass-alias for `genericShowInsert` constraints
class GenericShowInsert t r where
  genericShowInsert
    :: { ph :: String }
    -> Table t
    -> Array { | r }
    -> String

-- | the alias hides the auxiliary type parameter `rl`
instance genericShowInsertImpl ::
  ( TableColumnNames rl
  , RL.RowToList r rl
  , CanInsertColumnsIntoTable rl t
  , RowListLength rl
  ) =>
  GenericShowInsert t r
  where
  genericShowInsert { ph } table rs =
    let
      cols = joinWith ", " $ tableColumnNames (Proxy :: Proxy rl)
      len = rowListLength (Proxy :: Proxy rl)
      placeholders = mkPlaceholders ph 1 len $ Array.length rs
    in
      [ "INSERT INTO ", tableName table, " (", cols, ") VALUES ", placeholders, ";" ]
        # joinWith ""

mkPlaceholders :: String -> Int -> Int -> Int -> String
mkPlaceholders ph fstPH len n =
  if n <= 0 then ""
  else
    Array.range 0 (n - 1)
      # map ((*) len >>> (+) fstPH >>> phs)
      # joinWith ", "
  where
  phs i =
    Array.range i (i + len - 1)
      # map (\j -> ph <> show j)
      # joinWith ", "
      # \s -> "(" <> s <> ")"
