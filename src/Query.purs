module Query where

import Prelude

import Control.Monad.State (get, put)
import Data.Array ((:))
import Expr (class TableColumns, class ToCols, Col(..), Query(..), Table(..), freshId, tableColumns, toCols)
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Type.Row (RLProxy(..))

restrict ∷ ∀ s. Col s Boolean → Query s Unit
restrict (Col e) = Query do
  st ← get
  put $ st { restricts = e : st.restricts }

-- select 
--   ∷ ∀ s r rl res
--   . RL.RowToList r rl
--   ⇒ TableCols rl res
--   ⇒ Table r → Query s (Record res)
-- select (Table { name }) = do
--   id ← freshId
--   st ← Query get
--   let
--     table = { name, alias: name <> "_" <> show id }
--     res = tableCols table (RLProxy ∷ RLProxy rl)
--   Query $ put $ st { sources = table : st.sources }
--   pure res
select
  ∷ ∀ s r rl res i il
  . RL.RowToList r rl
  ⇒ TableColumns rl i
  ⇒ RL.RowToList i il
  ⇒ ToCols s i il res
  ⇒ Table r → Query s (Record res)
select (Table { name }) = do
  id ← freshId
  st ← Query get
  let
    aliased = { name, alias: name <> "_" <> show id }
    i = tableColumns aliased (RLProxy ∷ RLProxy rl)
  Query $ put $ st { sources = aliased : st.sources }
  toCols i (RLProxy ∷ RLProxy il)
