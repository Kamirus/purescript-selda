-- | Common to-string functions for SQL statements (SELECT, UPDATE, DELETE)
-- | shared between backends.
module Selda.Query.ShowStatement where

import Prelude

import Data.Exists (runExists)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Selda.Col (class GetCols, Col, getCols, showCol)
import Selda.Expr (ShowM, showExpr)
import Selda.Query.ShowQuery (showState)
import Selda.Query.Type (FullQuery, runQuery)
import Selda.Query.Utils (class TableToColsWithoutAlias, tableToColsWithoutAlias)
import Selda.Table (Table(..))
import Type.Proxy (Proxy(..))

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
