module Selda.SQLite3 where

import Prelude

import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Foreign (Foreign)
import SQLite3 (DBConnection, queryDB)
import Selda.Col (class GetCols)
import Selda.Expr (ShowM, showM)
import Selda.Query.ShowStatement (showQuery)
import Selda.Query.Type (FullQuery(..), runQuery)
import Simple.JSON (class ReadForeign, E, read)

showSQLite3
  ∷ ShowM
  → { params ∷ Array Foreign, nextIndex ∷ Int, strQuery ∷ String }
showSQLite3 = showM "?" 1

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
