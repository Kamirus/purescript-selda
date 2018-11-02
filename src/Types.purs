module Types where
  
import Prelude

import Control.Monad.State (State, get, put)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row as R
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Type.Row (RLProxy(..))

newtype Table ( r ∷ # Type ) = Table { name ∷ String }

type AliasedTable = { name ∷ String, alias ∷ String }

newtype Col a = Col { table ∷ AliasedTable, name ∷ String }
showCol ∷ ∀ a. Col a → String
showCol (Col { table, name }) = table.alias <> "." <> name

type GenState expr = 
  { sources ∷ Array AliasedTable
  , restricts ∷ Array (expr Boolean)
  , nextId ∷ Int
  }

initState ∷ ∀ expr. GenState expr
initState = 
  { sources: []
  , restricts: []
  , nextId: 0
  }

-- Table { name ∷ String, id ∷ Int } → { name ∷ Col String, id ∷ Col Int }
class TableCols (rl ∷ RowList) (r ∷ # Type) | rl → r where
  tableCols ∷ AliasedTable → RLProxy rl → Record r

instance tableColsNil ∷ TableCols RL.Nil () where
  tableCols _ _ = {}

instance tableColsCons
    ∷ ( IsSymbol sym
      , R.Lacks sym r'
      , R.Cons sym (Col t) r' r
      , TableCols tail r'
      )
    ⇒ TableCols (RL.Cons sym t tail) r
  where
  tableCols table _ = 
    let _sym = (SProxy ∷ SProxy sym) in
    let res' = tableCols table (RLProxy ∷ RLProxy tail) in
    Record.insert _sym (Col { table, name: reflectSymbol _sym }) res'

type Query expr a = State (GenState expr) a

freshId ∷ ∀ expr. Query expr Int
freshId = do
  st ← get
  put $ st { nextId = st.nextId + 1 }
  pure st.nextId
