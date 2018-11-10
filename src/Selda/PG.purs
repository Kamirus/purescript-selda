module Selda.PG
  ( withPG
  , class QueryRes
  , queryRes
  ) where

import Prelude

import Data.Array ((:))
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (class FromSQLRow, PoolConfiguration)
import Database.PostgreSQL as PG
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Prim.Row as R
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Selda.Col (Col)
import Selda.Query.Type (Query, runQuery)
import Type.Row (RLProxy(..))

{- 
For record { n1 ∷ Col s String, n2 ∷ Col s String, id ∷ Col s Int }
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
      , R.Cons sym (Col s t) i' i, R.Cons sym2 (Col s t2) i'' i
      )
    ⇒ QueryRes i (RL.Cons sym (Col s t) (RL.Cons sym2 (Col s t2) RL.Nil)) (Tuple t t2) o
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
    , cols: [ show col, show col2 ]
    }

else instance queryResCons
    ∷ ( IsSymbol sym
      , R.Lacks sym o'
      , R.Cons sym t o' o
      , R.Cons sym (Col s t) i' i
      , QueryRes i tail tup o'
      , FromSQLRow tup
      )
    ⇒ QueryRes i (RL.Cons sym (Col s t) tail) (Tuple t tup) o
  where
  queryRes i _ = 
    let 
      _sym = (SProxy ∷ SProxy sym)
      r = queryRes i (RLProxy ∷ RLProxy tail)
      f ( Tuple t tup ) = Record.insert _sym t $ r.f tup
      col = Record.get _sym i
    in
    { f
    , cols: show col : r.cols 
    }

rowToRecord
  ∷ ∀ i o tup il
  . RL.RowToList i il
  ⇒ QueryRes i il tup o
  ⇒ FromSQLRow tup
  ⇒ Record i → { f ∷ tup → Record o, cols ∷ Array String }
rowToRecord i = queryRes i (RLProxy ∷ RLProxy il)

withPG
  ∷ ∀ o i il tup
  . RL.RowToList i il
  ⇒ QueryRes i il tup o
  ⇒ FromSQLRow tup
  ⇒ (∀ s. Query s (Record i))
  → PoolConfiguration
  → Aff (Array (Record o))
withPG q dbconfig = do
  let
    (Tuple res st) = runQuery q
    from = aux " from " ", " (\t → t.name <> " " <> t.alias) st.sources
    wheres = aux " where " " AND " (\e → "(" <> show e <> ")") st.restricts
    { f, cols } = rowToRecord res
    q_str =
      "select " <> joinWith ", " cols
        <> from
        <> wheres
        <> ";"

  pool ← PG.newPool dbconfig
  PG.withConnection pool \conn → do
    liftEffect $ log q_str
    rows ← PG.query conn (PG.Query q_str) PG.Row0
    pure $ map f rows

aux ∷ ∀ a. String → String → (a → String) → Array a → String
aux beg sep f l = case l of
  [] → ""
  _ → beg <> (joinWith sep $ map f l)
