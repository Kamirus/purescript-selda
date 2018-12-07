module Selda.Table
  ( Column(..), showColumn
  , AliasedTable
  , Alias
  , Table(..)
  , class TableColumns, tableColumns
  , class TableColumnNames, tableColumnNames
  ) where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row as R
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Type.Row (RLProxy(..))

newtype Table ( r ∷ # Type ) = Table { name ∷ String }

type Alias = String

type AliasedTable = { name ∷ String, alias ∷ Alias }

newtype Column a = Column { namespace ∷ Alias, name ∷ String }

showColumn ∷ ∀ a. Column a → String
showColumn (Column { namespace, name }) = namespace <> "." <> name

-- Table { name ∷ String, id ∷ Int } → { name ∷ Column String, id ∷ Column Int }
class TableColumns (rl ∷ RowList) (r ∷ # Type) | rl → r where
  tableColumns ∷ AliasedTable → RLProxy rl → Record r

instance tableColumnsNil ∷ TableColumns RL.Nil () where
  tableColumns _ _ = {}

instance tableColumnsCons
    ∷ ( IsSymbol sym
      , R.Lacks sym r'
      , R.Cons sym (Column t) r' r
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
  tableColumnNames ∷ RLProxy rl → Array String
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
