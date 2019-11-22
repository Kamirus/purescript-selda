module Selda.SQLite3.Aff where

import Prelude

import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import SQLite3 (DBConnection, queryDB)
import Selda.Col (class GetCols)
import Selda.Query.ShowStatement (showQuery)
import Selda.Query.Type (FullQuery, runQuery)
import Selda.SQLite3 (showSQLite3)
import Simple.JSON (class ReadForeign, E, read)

query
  ∷ ∀ o i s
  . GetCols i
  ⇒ ReadForeign { | o }
  ⇒ DBConnection → FullQuery s { | i } → Aff (E (Array { | o }))
query conn q = do
  let
    (Tuple res _) = runQuery $ unwrap q
    { strQuery, params } = showSQLite3 $ showQuery q
  rows ← queryDB conn strQuery params
  pure $ read rows
