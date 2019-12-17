module Selda.SQLite3 where

import Foreign (Foreign)
import Selda.Expr (ShowM, showM)

showSQLite3
  ∷ ShowM
  → { params ∷ Array Foreign, nextIndex ∷ Int, strQuery ∷ String }
showSQLite3 = showM "?" 1

showSQLite3_ ∷ ∀ a. ShowM → (String → Array Foreign → a) → a
showSQLite3_ m k = k strQuery params
  where { strQuery, params } = showSQLite3 m
