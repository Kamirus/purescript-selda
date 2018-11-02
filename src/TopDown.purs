module TopDown where

import Prelude

import BottomUp (dbconfig, recordNames)
import Control.Monad.State (State, get, put, runState)
import Data.Array ((:))
import Data.Foldable (for_)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (class FromSQLRow)
import Database.PostgreSQL as PG
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Prim.Row as R
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Type.Row (RLProxy(..))

people ∷ Table ( name ∷ String , age ∷ Int , id ∷ Int )
people = Table { name: "people" }

newtype Table ( r ∷ # Type ) = Table { name ∷ String }

type AliasedTable = { name ∷ String, alias ∷ String }

newtype Col a = Col { table ∷ AliasedTable, name ∷ String }
showCol ∷ ∀ a. Col a → String
showCol (Col { table, name }) = table.alias <> "." <> name

type GenState = 
  { sources ∷ Array AliasedTable
  , nextId ∷ Int
  }

initState = 
  { sources: []
  , nextId: 0
  }

{-
Table { name ∷ String, id ∷ Int }
{ name ∷ Col String, id ∷ Col Int }
[ "id", "name" ]
((id ∷ Int) /\ (name ∷ String)) → { name ∷ String, id ∷ Int }
-}

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

class QueryRes (i ∷ # Type) (il ∷ RowList) tup (o ∷ # Type) | i il → tup o where
  queryRes
    ∷ FromSQLRow tup
    ⇒ Record i
    → RLProxy il
    → { f ∷ (tup → Record o)
      , cols ∷ Array String
      }

instance queryRes2
    ∷ ( IsSymbol sym , IsSymbol sym2
      , R.Lacks sym () , R.Lacks sym2 tmp
      , R.Cons sym t () tmp , R.Cons sym2 t2 tmp o
      , R.Cons sym (Col t) i' i, R.Cons sym2 (Col t) i'' i
      )
    ⇒ QueryRes i (RL.Cons sym (Col t) (RL.Cons sym2 (Col t2) RL.Nil)) (Tuple t t2) o
  where
  queryRes i _ = 
    let 
      _sym = (SProxy ∷ SProxy sym)
      _sym2 = (SProxy ∷ SProxy sym2)
      col = Record.get _sym i
      col2 = Record.get _sym2 i
      f (Tuple t t2) = 
        Record.insert _sym t {}
          # Record.insert _sym2 t2
    in
    { f
    , cols: [ showCol col, showCol col2 ]
    }

else instance queryResCons
    ∷ ( IsSymbol sym
      , R.Lacks sym o'
      , R.Cons sym t o' o
      , R.Cons sym (Col t) i' i
      , QueryRes i tail tup o'
      , FromSQLRow tup
      )
    ⇒ QueryRes i (RL.Cons sym (Col t) tail) (Tuple t tup) o
  where
  queryRes i _ = 
    let 
      _sym = (SProxy ∷ SProxy sym)
      r = queryRes i (RLProxy ∷ RLProxy tail)
      f ( Tuple t tup ) = Record.insert _sym t $ r.f tup
      col = Record.get _sym i
    in
    { f
    , cols: showCol col : r.cols 
    }

type Query a = State GenState a

select 
  ∷ ∀ r rl res
  . RL.RowToList r rl
  ⇒ TableCols rl res
  ⇒ Table r → Query (Record res)
select (Table { name }) = do
  id ← freshId
  st ← get
  let
    table = { name, alias: name <> show id }
    res = tableCols table (RLProxy ∷ RLProxy rl)
  put $ st { sources = table : st.sources }
  pure res

freshId ∷ Query Int
freshId = do
  st ← get
  put $ st { nextId = st.nextId + 1 }
  pure st.nextId

qqq
  ∷ State { sources ∷ Array { name ∷ String
                            , alias ∷ String
                            }
          , nextId ∷ Int
          }
          { id ∷ Col Int
          , name ∷ Col String
          }
qqq = do
  { id, name } ← select people
  pure $ { id, name }

rowToRecord
  ∷ ∀ i o tup il
  . RL.RowToList i il
  ⇒ QueryRes i il tup o
  ⇒ FromSQLRow tup
  ⇒ Record i → { f ∷ tup → Record o, cols ∷ Array String }
rowToRecord i = queryRes i (RLProxy ∷ RLProxy il)

runQuery 
  ∷ ∀ o i il tup
  . RL.RowToList i il
  ⇒ QueryRes i il tup o
  ⇒ FromSQLRow tup
  ⇒ Query (Record i)
  → Aff (Array (Record o))
runQuery q = do
  let
    (Tuple res st) = runState q initState
    tables = joinWith ", " $ map (\t → t.name <> " " <> t.alias) st.sources
    { f, cols } = rowToRecord res
    q_str = "select " <> joinWith ", " cols <> " from " <> tables <> ";"

  pool ← PG.newPool dbconfig
  PG.withConnection pool \conn → do
    -- rows ← query conn (PG.Query """
    --   select id, name from persons;
    -- """) Row0
    liftEffect $ log q_str
    rows ← PG.query conn (PG.Query q_str) PG.Row0
    pure $ map f rows
    -- liftEffect $ for_ rows \((id1 ∷ Int) /\ (id2 ∷ Int) /\ (name1 ∷ String) /\ (name2 ∷ String)) → do
    --   log $ name1 <> " " <> show id1 <> " " <> name2 <> " " <> show id2

main ∷ Effect Unit
main = launchAff_ $ do
  let
    q = do
      p1 ← select people
      p2 ← select people
      pure $ { id: p1.id, n1: p1.name, n2: p2.name }
  rows ← runQuery q
  liftEffect $ log "id\tn1\tn2"
  liftEffect $ for_ rows \{ id, n1, n2 } → log (show id <> "\t" <> n1 <> "\t" <> n2 )
