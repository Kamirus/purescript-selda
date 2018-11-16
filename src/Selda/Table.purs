module Selda.Table
  ( Column, showColumn
  , AliasedTable
  , Table(..)
  , class TableColumns
  , tableColumns
  ) where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row as R
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Type.Row (RLProxy(..))

newtype Table ( r ∷ # Type ) = Table { name ∷ String }

type AliasedTable = { name ∷ String, alias ∷ String }

newtype Column a = Column { table ∷ AliasedTable, name ∷ String }

showColumn ∷ ∀ a. Column a → String
showColumn (Column { table, name }) = table.alias <> "." <> name

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
      col = Column { table, name: reflectSymbol _sym }
    in
    Record.insert _sym col res'
