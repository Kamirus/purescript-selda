module Query where

import Prelude

import Control.Monad.State (get, put)
import Data.Array ((:))
import Expr (class TableCols, Col(..), Query(..), Table(..), freshId, tableCols)
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Type.Row (RLProxy(..))

restrict ∷ ∀ s. Col s Boolean → Query s Unit
restrict (Col e) = Query do
  st ← get
  put $ st { restricts = e : st.restricts }

select 
  ∷ ∀ s r rl res
  . RL.RowToList r rl
  ⇒ TableCols rl res
  ⇒ Table r → Query s (Record res)
select (Table { name }) = do
  id ← freshId
  st ← Query get
  let
    table = { name, alias: name <> "_" <> show id }
    res = tableCols table (RLProxy ∷ RLProxy rl)
  Query $ put $ st { sources = table : st.sources }
  pure res
