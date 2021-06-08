module Selda.SQLite3 where

import Prelude

import Foreign (Foreign)
import Selda.Col (Col(..))
import Selda.Aggr (class Coerce, unsafeFromCol)
import Selda.Expr (Expr(..), ShowM, showM)
import Simple.JSON (class WriteForeign, write)

-- | Lift a value `a` to a column expression using `WriteForeign a`.
-- | Please note that the value will be passed as a query parameter meaning it
-- | won't appear in the SQL query string as a serialized string, but as a
-- | placeholder with an index corresponding to the array of foreign parameters.
litSQLite3 ∷ ∀ col s a. WriteForeign a ⇒ Coerce col ⇒ a → col s a
litSQLite3 = unsafeFromCol <<< Col <<< EForeign <<< write

showSQLite3Query
  ∷ ShowM
  → String
showSQLite3Query = showSQLite3 >>> _.strQuery

showSQLite3
  ∷ ShowM
  → { params ∷ Array Foreign, nextIndex ∷ Int, strQuery ∷ String }
showSQLite3 = showM "?" 1

showSQLite3_ ∷ ∀ a. ShowM → (String → Array Foreign → a) → a
showSQLite3_ m k = k strQuery params
  where { strQuery, params } = showSQLite3 m
