module Selda.Table
  ( Column(..)
  , AliasedTable
  , Alias
  , StringSQL
  , Table(..)
  , tableName
  , class TableColumns, tableColumns
  , class TableColumnNames, tableColumnNames
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row as R
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Selda.Table.Constraint (class EraseConstraint)
import Type.Data.RowList (RLProxy(..))

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
data Table ( r ∷ # Type )
  = Table { name ∷ String }
  | Source Alias (Maybe Alias → StringSQL)

tableName ∷ ∀ t. Table t → String
tableName = case _ of
  Table { name } → name
  Source _ f → f Nothing

type Alias = String

type StringSQL = String

-- | Table-like source has two components:
-- | - body (SQL appearing after FROM or JOIN)
-- | - alias (used as a namespace for columns of the Table-like source)
type AliasedTable = { body ∷ String, alias ∷ Alias }

newtype Column a = Column { namespace ∷ Alias, name ∷ String }

-- Table { name ∷ String, id ∷ Int } → { name ∷ Column String, id ∷ Column Int }
class TableColumns (rl ∷ RowList Type) (r ∷ # Type) | rl → r where
  tableColumns ∷ ∀ t proxy. { alias ∷ Alias | t } → proxy rl → Record r

instance tableColumnsNil ∷ TableColumns RL.Nil () where
  tableColumns _ _ = {}

instance tableColumnsCons
    ∷ ( IsSymbol sym
      , R.Lacks sym r'
      , EraseConstraint t t'
      , R.Cons sym (Column t') r' r
      , TableColumns tail r'
      )
    ⇒ TableColumns (RL.Cons sym t tail) r
  where
  tableColumns table _ = 
    let
      _sym = (SProxy ∷ SProxy sym)
      res' = tableColumns table (RLProxy ∷ RLProxy tail)
      col = Column { namespace: table.alias, name: reflectSymbol _sym }
    in
    Record.insert _sym col res'

class TableColumnNames rl where
  tableColumnNames ∷ forall proxy. proxy rl → Array String
instance tableColumnNamesHead ∷ TableColumnNames RL.Nil where
  tableColumnNames _ = []
else instance tableColumnNamesCons
    ∷ ( IsSymbol sym , TableColumnNames tail )
    ⇒ TableColumnNames (RL.Cons sym t tail)
  where
  tableColumnNames _ = tableColumnNames _tail <> [reflectSymbol _sym]
    where
    _tail = (RLProxy ∷ RLProxy tail)
    _sym = (SProxy ∷ SProxy sym)
