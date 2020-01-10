-- | Common to-string functions for SQL statements
-- | (SELECT, UPDATE, DELETE, INSERT) shared between backends.
module Selda.Query.ShowStatement where

import Prelude

import Data.Array as Array
import Data.Exists (runExists)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Prim.RowList as RL
import Selda.Col (class GetCols, Col, getCols, showCol)
import Selda.Expr (ShowM, showExpr)
import Selda.Query.PrettyPrint (PrettyM, ppState)
import Selda.Query.Type (FullQuery, GenState(..), runQuery)
import Selda.Query.Utils (class RowListLength, class TableToColsWithoutAlias, rowListLength, tableToColsWithoutAlias)
import Selda.Table (class TableColumnNames, Table(..), tableColumnNames)
import Selda.Table.Constraint (class CanInsertColumnsIntoTable)
import Text.Pretty (render)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

ppQuery ∷ ∀ i s. GetCols i ⇒ FullQuery s { | i } → PrettyM
ppQuery q = ppState st
  where
    (Tuple res (GenState st')) = runQuery $ unwrap q
    st = st' { cols = getCols res }

showQuery ∷ ∀ i s. GetCols i ⇒ FullQuery s (Record i) → ShowM
showQuery q = render 0 <$> ppQuery q

showDeleteFrom
  ∷ ∀ t s r
  . TableToColsWithoutAlias s t r
  ⇒ Table t → ({ | r } → Col s Boolean) → ShowM
showDeleteFrom table@(Table { name }) pred = do
  let recordWithCols = tableToColsWithoutAlias (Proxy ∷ Proxy s) table
  pred_str ← showCol $ pred recordWithCols
  pure $ "DELETE FROM " <> name <> " WHERE " <> pred_str

showUpdate
  ∷ ∀ t s r
  . TableToColsWithoutAlias s t r
  ⇒ GetCols r
  ⇒ Table t → ({ | r } → Col s Boolean) → ({ | r } → { | r }) → ShowM
showUpdate table@(Table { name }) pred up = do
  let
    recordWithCols = tableToColsWithoutAlias (Proxy ∷ Proxy s) table
    f (Tuple n e) = do 
      s ← runExists showExpr e
      pure $ n <> " = " <> s 
  pred_str ← showCol $ pred recordWithCols
  vals ← joinWith ", " <$> (traverse f $ getCols $ up recordWithCols)
  pure $ "UPDATE " <> name <> " SET " <> vals <> " WHERE " <> pred_str

-- | typeclass-alias for `genericShowInsert` constraints
class GenericShowInsert t r where
  genericShowInsert
    ∷ { ph ∷ String }
    → Table t
    → Array { | r }
    → String

-- | the alias hides the auxiliary type parameter `rl`
instance genericShowInsertImpl
    ∷ ( TableColumnNames rl
      , RL.RowToList r rl
      , CanInsertColumnsIntoTable rl t
      , RowListLength rl
      ) ⇒ GenericShowInsert t r
  where
  genericShowInsert { ph } (Table { name }) rs =
    let
      cols = joinWith ", " $ tableColumnNames (RLProxy ∷ RLProxy rl)
      len = rowListLength (RLProxy ∷ RLProxy rl)
      placeholders = mkPlaceholders ph 1 len $ Array.length rs
    in
      ["INSERT INTO ", name, " (", cols, ") VALUES ", placeholders, ";"]
        # joinWith ""

mkPlaceholders ∷ String → Int → Int → Int → String
mkPlaceholders ph fstPH len n = if n <= 0 then "" else
  Array.range 0 (n - 1)
    # map ((*) len >>> (+) fstPH >>> phs)
    # joinWith ", "
  where
    phs i =
      Array.range i (i + len - 1)
        # map (\j → ph <> show j)
        # joinWith ", "
        # \s → "(" <> s <> ")"
