module Selda.SQLite3.Aff where

import Prelude

import Effect.Aff (Aff)
import SQLite3 (DBConnection)
import Selda.Col (class GetCols)
import Selda.Query.Class (runSelda)
import Selda.Query.Type (FullQuery)
import Selda.Query.Utils (class MapR, UnCol_)
import Selda.SQLite3.Class (BackendSQLite3Class)
import Selda.SQLite3.Class as S
import Simple.JSON (class ReadForeign, E)

query
  :: forall i o
   . GetCols i
  => MapR UnCol_ i o
  => ReadForeign { | o }
  => DBConnection
  -> FullQuery BackendSQLite3Class { | i }
  -> Aff (E (Array { | o }))
query conn q = runSelda conn $ S.query q
