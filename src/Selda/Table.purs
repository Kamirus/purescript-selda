module Selda.Table
  ( Column(..)
  , AliasedTable
  , Alias
  , StringSQL
  , showColumn
  , showColumnName
  , Table(..)
  , tableName
  , class TableColumns
  , tableColumns
  , class TableColumnNames
  , tableColumnNames
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace) as String
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as R
import Prim.RowList as RL
import Record as Record
import Selda.Table.Constraint (class EraseConstraint)
import Type.Proxy (Proxy(..))

-- | Represents table-like SQL sources.
-- | 
-- | `Table` constructor is used for simple cases.
-- | **It requires a table name, which is not namespaced!**
-- | The given `name` is used as an alias prefix for its columns,
-- | meaning each column is namespaced using a full alias
-- | that is a combination of the table's `name` and a unique number
-- | supplied during query generation.
-- | 
-- | `Source` covers more flexible cases.
-- | It requires an alias prefix and a way to create an SQL string
-- | (which is used in the `FROM` or `JOIN` SQL clause)
-- | with or without a full alias (which is a combination of an alias prefix
-- | and a unique number supplied during query generation).
data Table :: Row Type -> Type
data Table r
  = Table { name :: String }
  | Source Alias (Maybe Alias -> StringSQL)

tableName :: forall t. Table t -> String
tableName = case _ of
  Table { name } -> name
  Source _ f -> f Nothing

type Alias = String

type StringSQL = String

-- | Table-like source has two components:
-- | - body (SQL appearing after FROM or JOIN)
-- | - alias (used as a namespace for columns of the Table-like source)
type AliasedTable = { body :: String, alias :: Alias }

newtype Column :: forall k. k -> Type
newtype Column a = Column { namespace :: Alias, name :: String }

-- Table { name ∷ String, id ∷ Int } → { name ∷ Column String, id ∷ Column Int }
class TableColumns :: RL.RowList Type -> Row Type -> Constraint
class TableColumns rl r | rl -> r where
  tableColumns :: forall t proxy. { alias :: Alias | t } -> proxy rl -> Record r

instance tableColumnsNil :: TableColumns RL.Nil () where
  tableColumns _ _ = {}

instance tableColumnsCons ::
  ( IsSymbol sym
  , R.Lacks sym r'
  , EraseConstraint t t'
  , R.Cons sym (Column t') r' r
  , TableColumns tail r'
  ) =>
  TableColumns (RL.Cons sym t tail) r
  where
  tableColumns table _ =
    let
      _sym = (Proxy :: Proxy sym)
      res' = tableColumns table (Proxy :: Proxy tail)
      col = Column
        { namespace: table.alias
        , name: showColumnName $ reflectSymbol _sym
        }
    in
      Record.insert _sym col res'

class TableColumnNames :: RL.RowList Type -> Constraint
class TableColumnNames rl where
  tableColumnNames :: forall proxy. proxy rl -> Array String

instance tableColumnNamesHead :: TableColumnNames RL.Nil where
  tableColumnNames _ = []
else instance tableColumnNamesCons ::
  ( IsSymbol sym
  , TableColumnNames tail
  ) =>
  TableColumnNames (RL.Cons sym t tail)
  where
  tableColumnNames _ = tableColumnNames _tail
    <> [ showColumnName $ reflectSymbol _sym ]
    where
    _tail = (Proxy :: Proxy tail)
    _sym = (Proxy :: Proxy sym)

showColumnName :: String -> String
showColumnName name = "\"" <> name <> "\""

showColumn :: forall a. Column a -> String
showColumn (Column { namespace, name })
  | namespace == "" = name
  | otherwise = namespace <> "." <> name

-- | Escape double quotes in an SQL identifier.
escapeQuotes :: String -> String
escapeQuotes = String.replace (String.Pattern "\"") (String.Replacement "\"\"")
