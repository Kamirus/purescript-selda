module Selda.SQLite3.Class
  ( class MonadSeldaSQLite3
  , query
  , query1
  , insert_
  , deleteFrom
  , update
  , BackendSQLite3Class
  ) where

import Prelude

import Control.Monad.Reader (ask)
import Data.Array (null)
import Data.Array as Array
import Data.Either (either)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Effect.Aff (throwError)
import Effect.Aff.Class (liftAff)
import Foreign (Foreign, ForeignError, MultipleErrors)
import Heterogeneous.Folding (class HFoldl)
import SQLite3 (DBConnection, queryDB)
import Selda.Col (Col, class GetCols)
import Selda.Query.Class (class GenericDelete, class GenericInsert, class GenericQuery, class GenericUpdate, class MonadSelda, genericDelete, genericInsert, genericInsert_, genericQuery, genericUpdate)
import Selda.Query.ShowStatement (class GenericShowInsert, showQuery, showDeleteFrom, showUpdate)
import Selda.Query.Type (FullQuery)
import Selda.Query.Utils (class MapR, class TableToColsWithoutAlias, class ToForeign, RecordToArrayForeign, UnCol_)
import Selda.SQLite3 (showSQLite3_)
import Selda.Table (Table)
import Simple.JSON (class ReadForeign, class WriteForeign, read, write)
import Type.Proxy (Proxy(..))

type B = BackendSQLite3Class

data BackendSQLite3Class

class
  ( MonadSelda m (NonEmptyList ForeignError) DBConnection
  ) <=
  MonadSeldaSQLite3 m

instance monadSeldaSQLite3Instance ::
  MonadSelda m MultipleErrors DBConnection =>
  MonadSeldaSQLite3 m

query
  :: forall m i o
   . GenericQuery BackendSQLite3Class m i o
  => FullQuery B { | i }
  -> m (Array { | o })
query = genericQuery (Proxy :: Proxy BackendSQLite3Class)

query1
  :: forall m i o
   . GenericQuery BackendSQLite3Class m i o
  => FullQuery B { | i }
  -> m (Maybe { | o })
query1 q = query q <#> Array.head

instance genericQuerySQLite3 ::
  ( MonadSeldaSQLite3 m
  , ReadForeign { | o }
  , MapR UnCol_ i o
  , GetCols i
  ) =>
  GenericQuery BackendSQLite3Class m i o
  where
  genericQuery _ q = do
    rows <- execSQLite3 # showSQLite3_ (showQuery q)
    either throwError pure (read rows)

insert_
  :: forall m t r
   . GenericInsert BackendSQLite3Class m t r
  => Table t
  -> Array { | r }
  -> m Unit
insert_ = genericInsert (Proxy :: Proxy BackendSQLite3Class)

instance sqlite3ToForeign :: WriteForeign a => ToForeign BackendSQLite3Class a where
  toForeign _ = write

instance genericInsertSQLite3 ::
  ( HFoldl (RecordToArrayForeign BackendSQLite3Class)
      (Array Foreign)
      { | r }
      (Array Foreign)
  , MonadSeldaSQLite3 m
  , GenericShowInsert t r
  ) =>
  GenericInsert BackendSQLite3Class m t r
  where
  genericInsert proxy table l = when (not $ null l) do
    genericInsert_ { exec: execSQLite3_, ph: "?" } proxy table l

deleteFrom
  :: forall t r m
   . GenericDelete BackendSQLite3Class m t r
  => Table t
  -> ({ | r } -> Col B Boolean)
  -> m Unit
deleteFrom = genericDelete (Proxy :: Proxy BackendSQLite3Class)

instance genericDeleteSQLite3 ::
  ( TableToColsWithoutAlias B t r
  , MonadSeldaSQLite3 m
  ) =>
  GenericDelete BackendSQLite3Class m t r
  where
  genericDelete _ table pred =
    execSQLite3_ # showSQLite3_ (showDeleteFrom table pred)

update
  :: forall t r m
   . GenericUpdate BackendSQLite3Class m t r
  => Table t
  -> ({ | r } -> Col B Boolean)
  -> ({ | r } -> { | r })
  -> m Unit
update = genericUpdate (Proxy :: Proxy BackendSQLite3Class)

instance genericUpdateSQLite3 ::
  ( TableToColsWithoutAlias B t r
  , GetCols r
  , MonadSeldaSQLite3 m
  ) =>
  GenericUpdate BackendSQLite3Class m t r
  where
  genericUpdate _ table pred up =
    execSQLite3_ # showSQLite3_ (showUpdate table pred up)

-- | Utility function to execute a given query (as String) with parameters
execSQLite3 :: forall m. MonadSeldaSQLite3 m => String -> Array Foreign -> m Foreign
execSQLite3 q params = do
  conn <- ask
  liftAff $ queryDB conn q params

-- | Utility function to execute a given query (as String) with parameters and discard the result
execSQLite3_ :: forall m. MonadSeldaSQLite3 m => String -> Array Foreign -> m Unit
execSQLite3_ q l = when (q /= "") $ void $ execSQLite3 q l
