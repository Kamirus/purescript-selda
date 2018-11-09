module Query where

import Prelude

import Control.Monad.State (get, put)
import Data.Array ((:))
import Expr (Expr, Table(..))
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Type.Row (RLProxy(..))
import Types (class TableCols, Query, freshId, tableCols)

restrict ∷ Expr Boolean → Query Unit
restrict e = do
  st ← get
  put $ st { restricts = e : st.restricts }

select 
  ∷ ∀ r rl res
  . RL.RowToList r rl
  ⇒ TableCols rl res
  ⇒ Table r → Query (Record res)
select (Table { name }) = do
  id ← freshId
  st ← get
  let
    table = { name, alias: name <> "_" <> show id }
    res = tableCols table (RLProxy ∷ RLProxy rl)
  put $ st { sources = table : st.sources }
  pure res
