module Types where
  
import Prelude

import Control.Monad.State (State, get, put)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Expr (AliasedTable, Col(..), Expr)
import Prim.Row as R
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Type.Row (RLProxy(..))

type GenState = 
  { sources ∷ Array AliasedTable
  , restricts ∷ Array (Expr Boolean)
  , nextId ∷ Int
  }

initState ∷ GenState
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

type Query a = State GenState a

freshId ∷ Query Int
freshId = do
  st ← get
  put $ st { nextId = st.nextId + 1 }
  pure st.nextId
