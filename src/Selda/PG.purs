module Selda.PG
  ( withPG
  , class BuildPGHandler
  , buildPGHandler
  ) where

import Prelude

import Data.Array (foldl, reverse, (:))
import Data.Array as Array
import Data.Exists (Exists, runExists)
import Data.Maybe (Maybe(..))
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
import Selda.Col (class ExtractCols, Col, getCols)
import Selda.Expr (Expr, showExpr)
import Selda.Query.Type (Source(..), Query, SQL(..), GenState, runQuery)
import Selda.Table (Alias)
import Type.Row (RLProxy(..))

{- 
For record
  { n1 ∷ Col s String, n2 ∷ Col s String, id ∷ Col s Int }
build function
  \Tuple int (Tuple string1 string2) → { id: int, n1: string1, n2: string2 }
-}
pgHandler
  ∷ ∀ i o tuple il
  . RL.RowToList i il
  ⇒ BuildPGHandler il tuple o
  ⇒ FromSQLRow tuple
  ⇒ Record i → (tuple → Record o)
pgHandler _ = buildPGHandler (RLProxy ∷ RLProxy il)

class BuildPGHandler (il ∷ RowList) tup (o ∷ # Type) | il → tup o where
  buildPGHandler
    ∷ FromSQLRow tup
    ⇒ RLProxy il
    → (tup → Record o)

instance buildPGHandlerHead
    ∷ ( IsSymbol sym
      , R.Lacks sym ()
      , R.Cons sym t () o
      )
    ⇒ BuildPGHandler (RL.Cons sym (Col s t) RL.Nil) (Tuple t Unit) o
  where
  buildPGHandler _ =
    \(Tuple t _) → Record.insert (SProxy ∷ SProxy sym) t {}
else instance buildPGHandlerCons
    ∷ ( IsSymbol sym
      , R.Lacks sym o'
      , R.Cons sym t o' o
      , BuildPGHandler tail tup o'
      , FromSQLRow tup
      )
    ⇒ BuildPGHandler (RL.Cons sym (Col s t) tail) (Tuple t tup) o
  where
  buildPGHandler _ =
    let f' = buildPGHandler (RLProxy ∷ RLProxy tail) in
    \(Tuple t tup) → Record.insert (SProxy ∷ SProxy sym) t $ f' tup

withPG
  ∷ ∀ o i il tup s
  . RL.RowToList i il
  ⇒ BuildPGHandler il tup o
  ⇒ ExtractCols i il
  ⇒ FromSQLRow tup
  ⇒ PoolConfiguration
  → Query s (Record i)
  → Aff (Array (Record o))
withPG dbconfig q = do
  let
    (Tuple res st') = runQuery q
    st = st' { cols = getCols res }
    q_str = showState st
  liftEffect $ log q_str
  pool ← PG.newPool dbconfig
  PG.withConnection pool \conn → do
    rows ← PG.query conn (PG.Query q_str) PG.Row0
    pure $ map (pgHandler res) rows

showState ∷ GenState → String
showState { cols, sources, restricts } = 
  showCols cols
    <> showSources sources
    <> showRestricts restricts

showCols ∷ Array (Tuple Alias (Exists Expr)) → String
showCols = case _ of
  [] → ""
  xs → "select " <> (joinWith ", " $ map showAliasedCol xs)

showSources ∷ Array Source → String
showSources sources = case Array.uncons $ reverse sources of
  Nothing →
    ""
  Just { head: h@(Product t), tail } →
    " from " <> foldl (\acc x → acc <> sepFor x <> showSource x) (showSource h) tail
  Just { head: LeftJoin t _, tail } →
    -- join on the first place, drop it and interpret as Product
    showSources $ Product t : tail

showRestricts ∷ Array (Expr Boolean) → String
showRestricts = case _ of
  [] → ""
  xs → " where " <> (joinWith " AND " $ map (\e → "(" <> showExpr e <> ")") xs)

showSQL ∷ SQL → String
showSQL = case _ of
  FromTable t →
    t.name <> " " <> t.alias
  SubQuery alias state → 
    "(" <> showState state <> ") " <> alias

sepFor ∷ Source → String
sepFor = case _ of
  Product _ → ", "
  LeftJoin _ _ → " left join "

showSource ∷ Source → String
showSource = case _ of
  Product t → showSQL t
  LeftJoin t e → showSQL t <> " on (" <> showExpr e <> ")"

showAliasedCol ∷ Tuple Alias (Exists Expr) → String
showAliasedCol (Tuple alias ee) = runExists showExpr ee <> " as " <> alias

aux ∷ ∀ a. String → String → (a → String) → Array a → String
aux beg sep f l = case l of
  [] → ""
  _ → beg <> (joinWith sep $ map f l)
