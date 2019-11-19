module Selda.SQLite3 where

import Prelude

import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
import SQLite3 (queryDB)
import Selda.Expr (ShowM, showM)
import Selda.Query.ShowStatement (showQuery)
import Selda.Query.Type (runQuery)

showSQLite3
  ∷ ShowM
  → { params ∷ Array Foreign, nextIndex ∷ Int, strQuery ∷ String }
showSQLite3 = showM "?" 1

-- query
--   ∷ ∀ o i s m
--   . GetCols i
--   ⇒ Monad m
--   ⇒ FullQuery s { | i } → m (Array { | o })
-- query backend toRecord q = do
--   let
--     (Tuple res _) = runQuery $ unwrap q
--     { strQuery, params } = showSQLite3 $ showQuery q
--   rows ← backend strQuery params
--   pure $ map toRecord rows
--   pure $ map (colsToPGHandler (Proxy ∷ Proxy s) res) rows
