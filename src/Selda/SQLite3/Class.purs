module Selda.SQLite3.Class
  ( class MonadSeldaSQLite3
  , query
  , query'
  , query1
  , query1'
  , insert_
  , insert_'
  , deleteFrom
  , update
  , BackendSQLite3Class
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ask)
import Data.Array (null)
import Data.Array as Array
import Data.Either (either)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Effect.Aff (throwError)
import Effect.Aff.Class (liftAff)
import Foreign (Foreign, ForeignError, MultipleErrors, readArray, F)
import Heterogeneous.Folding (class HFoldl, hfoldl)
import SQLite3 (DBConnection, queryDB)
import Selda.Col (Col, class GetCols)
import Selda.Query.Class (class GenericDelete, class GenericInsert, class GenericQuery, class GenericUpdate, class MonadSelda, genericDelete, genericInsert, genericInsert_, genericQuery, genericUpdate)
import Selda.Query.ShowStatement (class GenericShowInsert, showQuery, showDeleteFrom, showUpdate)
import Selda.Query.Type (FullQuery)
import Selda.Query.Utils (class MapR, class TableToColsWithoutAlias, class ToForeign, RecordToArrayForeign(..), UnCol_)
import Selda.SQLite3 (showSQLite3_)
import Selda.Table (Table)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, write)
import Type.Proxy (Proxy(..))

type B = BackendSQLite3Class

data BackendSQLite3Class

class
  ( MonadSelda m (NonEmptyList ForeignError) DBConnection
  ) <= MonadSeldaSQLite3 m

instance monadSeldaSQLite3Instance
  ∷ MonadSelda m MultipleErrors DBConnection
  ⇒ MonadSeldaSQLite3 m

query
  ∷ ∀ m i o
  . GenericQuery BackendSQLite3Class Foreign F m i o
  ⇒ ReadForeign { | o }
  ⇒ FullQuery B { | i } → m (Array { | o })
query q = query' q readImpl

query'
  ∷ ∀ m i o
  . GenericQuery BackendSQLite3Class Foreign F m i o
  ⇒ FullQuery B { | i } → (Foreign → F { | o }) → m (Array { | o })
query' q decode = genericQuery (Proxy ∷ Proxy BackendSQLite3Class) q decode

query1
  ∷ ∀ m i o
  . GenericQuery BackendSQLite3Class Foreign F m i o
  ⇒ ReadForeign { | o }
  ⇒ FullQuery B { | i }
  → m (Maybe { | o })
query1 q = query1' q readImpl

query1'
  ∷ ∀ m i o
  . GenericQuery BackendSQLite3Class Foreign F m i o
  ⇒ FullQuery B { | i }
  → (Foreign → F { | o })
  → m (Maybe { | o })
query1' q decodeVal = query' q decodeVal <#> Array.head

instance genericQuerySQLite3
    ∷ ( MonadSeldaSQLite3 m
      , MapR UnCol_ i o
      , GetCols i
      ) ⇒ GenericQuery BackendSQLite3Class Foreign F m i o
  where
  genericQuery _ q decodeRow = do
    rows ← execSQLite3 # showSQLite3_ (showQuery q)
    either throwError pure $ runExcept do
      row <- readArray rows
      traverse decodeRow row

insert_
  ∷ ∀ m t r
  . HFoldl (RecordToArrayForeign BackendSQLite3Class)
       (Array Foreign) { | r } (Array Foreign)
  ⇒ GenericInsert BackendSQLite3Class m t r
  ⇒ Table t → Array { | r } → m Unit
insert_ t xs = insert_' t encode xs
  where
  encode :: { | r } -> Array Foreign
  encode = hfoldl (RecordToArrayForeign b) ([] ∷ Array Foreign)

  b = Proxy :: Proxy BackendSQLite3Class

insert_'
  ∷ ∀ m t r
  . GenericInsert BackendSQLite3Class m t r
  ⇒ Table t → ({ | r } -> Array Foreign) -> Array { | r } → m Unit
insert_' = genericInsert (Proxy ∷ Proxy BackendSQLite3Class)

instance sqlite3ToForeign ∷ WriteForeign a ⇒ ToForeign BackendSQLite3Class a where
  toForeign _ = write

instance genericInsertSQLite3
    ∷ ( MonadSeldaSQLite3 m
      , GenericShowInsert t r
      ) ⇒ GenericInsert BackendSQLite3Class m t r
  where
  genericInsert proxy table encode l = when (not $ null l) do
    genericInsert_ { exec: execSQLite3_, ph: "?" } proxy table encode l

deleteFrom
  ∷ ∀ t r m
  . GenericDelete BackendSQLite3Class m t r
  ⇒ Table t → ({ | r } → Col B Boolean) → m Unit
deleteFrom = genericDelete (Proxy ∷ Proxy BackendSQLite3Class)

instance genericDeleteSQLite3
    ∷ ( TableToColsWithoutAlias B t r
      , MonadSeldaSQLite3 m
      ) ⇒ GenericDelete BackendSQLite3Class m t r
  where
  genericDelete _ table pred =
    execSQLite3_ # showSQLite3_ (showDeleteFrom table pred)

update
  ∷ ∀ t r m
  . GenericUpdate BackendSQLite3Class m t r
  ⇒ Table t → ({ | r } → Col B Boolean) → ({ | r } → { | r }) → m Unit
update = genericUpdate (Proxy ∷ Proxy BackendSQLite3Class)

instance genericUpdateSQLite3
    ∷ ( TableToColsWithoutAlias B t r
      , GetCols r
      , MonadSeldaSQLite3 m
      ) ⇒ GenericUpdate BackendSQLite3Class m t r
  where
  genericUpdate _ table pred up =
    execSQLite3_ # showSQLite3_ (showUpdate table pred up)

-- | Utility function to execute a given query (as String) with parameters
execSQLite3 ∷ ∀ m. MonadSeldaSQLite3 m ⇒ String → Array Foreign → m Foreign
execSQLite3 q params = do
  conn ← ask
  liftAff $ queryDB conn q params

-- | Utility function to execute a given query (as String) with parameters and discard the result
execSQLite3_ ∷ ∀ m. MonadSeldaSQLite3 m ⇒ String → Array Foreign → m Unit
execSQLite3_ q l = when (q /= "") $ void $ execSQLite3 q l
