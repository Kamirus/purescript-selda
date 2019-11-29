module Selda.SQLite3.Class where

import Prelude

import Control.Monad.Reader (ask)
import Data.Either (either)
import Data.List.Types (NonEmptyList)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (throwError)
import Effect.Aff.Class (liftAff)
import Foreign (ForeignError, MultipleErrors)
import SQLite3 (DBConnection, queryDB)
import Selda.Col (class GetCols)
import Selda.Query.Class (class GenericQuery, class MonadSelda, genericQuery)
import Selda.Query.ShowStatement (showQuery)
import Selda.Query.Type (FullQuery, runQuery)
import Selda.Query.Utils (class MapR, UnCol_)
import Selda.SQLite3 (showSQLite3)
import Simple.JSON (class ReadForeign, read)
import Type.Proxy (Proxy(..))

data BackendSQLite3Class

class 
  ( MonadSelda m (NonEmptyList ForeignError) DBConnection
  ) <= MonadSeldaSQLite3 m

instance monadSeldaSQLite3Instance
  ∷ MonadSelda m MultipleErrors DBConnection
  ⇒ MonadSeldaSQLite3 m

query
  ∷ ∀ m s i o
  . GenericQuery BackendSQLite3Class m s i o
  ⇒ FullQuery s { | i } → m (Array { | o })
query = genericQuery (Proxy ∷ Proxy BackendSQLite3Class)

instance genericQuerySQLite3
    ∷ ( GetCols i
      , MapR UnCol_ i o
      , ReadForeign { | o }
      , MonadSeldaSQLite3 m
      ) ⇒ GenericQuery BackendSQLite3Class m s i o
  where
  genericQuery _ q = do
    let
      (Tuple res _) = runQuery $ unwrap q
      { strQuery, params } = showSQLite3 $ showQuery q
    conn ← ask
    rows ← liftAff $ queryDB conn strQuery params
    either throwError pure (read rows)
