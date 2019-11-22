module Selda.SQLite3 where

import Foreign (Foreign)
import Selda.Expr (ShowM, showM)

showSQLite3
  ∷ ShowM
  → { params ∷ Array Foreign, nextIndex ∷ Int, strQuery ∷ String }
showSQLite3 = showM "?" 1
