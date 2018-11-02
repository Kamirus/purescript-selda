module PG.PG where

import Prelude

import BottomUp (dbconfig)
import Control.Monad.State (runState)
import Data.Array ((:))
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (class FromSQLRow)
import Database.PostgreSQL as PG
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import PG.Exp (EI(..))
import Prim.Row as R
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Type.Row (RLProxy(..))
import Types (Col, Query, initState, showCol)

type PGQuery a = Query EI a

{- 
For record { n1 ∷ Col String, n2 ∷ Col String, id ∷ Col Int }
produce [ "alias1.id", "alias1.name", "alias2.name" ]  notice order (id, n1, n2) not (n1, n2, id)
and function used to retrieve values from postgresql client
  \Tuple int (Tuple string1 string2) → { id: int, n1: string1, n2: string2 }
-}
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
  ⇒ PGQuery (Record i)
  → Aff (Array (Record o))
runQuery q = do
  let
    (Tuple res st) = runState q initState
    tables = joinWith ", " $ map (\t → t.name <> " " <> t.alias) st.sources
    wheres = joinWith " AND " $ map (\(EI e) → "(" <> e <> ")") st.restricts
    { f, cols } = rowToRecord res
    q_str =
      "select " <> joinWith ", " cols
        <> " from " <> tables
        <> " where " <> wheres
        <> ";"

  pool ← PG.newPool dbconfig
  PG.withConnection pool \conn → do
    liftEffect $ log q_str
    rows ← PG.query conn (PG.Query q_str) PG.Row0
    pure $ map f rows
