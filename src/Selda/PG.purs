module Selda.PG
  ( withPG
  , class BuildPGHandler
  , class ValidateSInCols
  , buildPGHandler
  , class ColsToPGHandler, colsToPGHandler
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
import Type.Proxy (Proxy(..))
import Type.Row (RLProxy(..))

{- 
For record
  { n1 ∷ Col s String, n2 ∷ Col s String, id ∷ Col s Int }
build function
  \Tuple int (Tuple string1 string2) → { id: int, n1: string1, n2: string2 }
-}
class ColsToPGHandler s i tup o | s i → tup o where
  colsToPGHandler ∷ Proxy s → { | i } → (tup → { | o })
instance colsToPGHandlerI
    ∷ ( RL.RowToList i il
      , ValidateSInCols s il ol
      , BuildPGHandler ol tup o
      , FromSQLRow tup
      )
    ⇒ ColsToPGHandler s i tup o
  where
  colsToPGHandler _ i = buildPGHandler (RLProxy ∷ RLProxy ol)

class ValidateSInCols s (il ∷ RowList) (ol ∷ RowList) | s il → ol
instance rLUnColNil ∷ ValidateSInCols s RL.Nil RL.Nil
else instance rLUnColCons
  ∷ ValidateSInCols s tail tail'
  ⇒ ValidateSInCols s (RL.Cons sym (Col s t) tail) (RL.Cons sym t tail')

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
    ⇒ BuildPGHandler (RL.Cons sym t RL.Nil) (Tuple t Unit) o
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
    ⇒ BuildPGHandler (RL.Cons sym t tail) (Tuple t tup) o
  where
  buildPGHandler _ =
    let f' = buildPGHandler (RLProxy ∷ RLProxy tail) in
    \(Tuple t tup) → Record.insert (SProxy ∷ SProxy sym) t $ f' tup

-- insert
--   ∷ ∀ r
--   ⇒ PoolConfiguration → Table r → Array { | r } → Aff (Array { | r })
-- insert dbconfig = case _ of
--   [] → pure []
--   xs → do
--     let q_str = "INSERT INTO "

--     pool ← PG.newPool dbconfig
--     PG.withConnection pool \conn → do
--       rows ← PG.query conn (PG.Query q_str) PG.Row0
--       pure $ map (pgHandler res) rows

withPG
  ∷ ∀ o i il tup s
  . RL.RowToList i il
  ⇒ ColsToPGHandler s i tup o
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
  -- liftEffect $ log q_str
  pool ← PG.newPool dbconfig
  PG.withConnection pool \conn → do
    rows ← PG.query conn (PG.Query q_str) PG.Row0
    pure $ map (colsToPGHandler (Proxy ∷ Proxy s) res) rows

showState ∷ GenState → String
showState { cols, sources, restricts, aggr } = 
  showCols cols
    <> showSources sources
    <> showRestricts restricts
    <> showGrouping aggr

showCols ∷ Array (Tuple Alias (Exists Expr)) → String
showCols = case _ of
  [] → ""
  xs → "SELECT " <> (joinWith ", " $ map showAliasedCol xs)

showSources ∷ Array Source → String
showSources sources = case Array.uncons $ reverse sources of
  Nothing →
    ""
  Just { head: h@(Product t), tail } →
    " FROM " <> foldl (\acc x → acc <> sepFor x <> showSource x) (showSource h) tail
  Just { head: LeftJoin t _, tail } →
    -- join on the first place, drop it and interpret as Product
    showSources $ Product t : tail

showRestricts ∷ Array (Expr Boolean) → String
showRestricts = case _ of
  [] → ""
  xs → " WHERE " <> (joinWith " AND " $ map (\e → "(" <> showExpr e <> ")") xs)

showGrouping ∷ Array (Exists Expr) → String
showGrouping = case _ of
  [] → ""
  xs → " GROUP BY " <> (joinWith ", " $ map (runExists showExpr) xs)

showSQL ∷ SQL → String
showSQL = case _ of
  FromTable t →
    t.name <> " " <> t.alias
  SubQuery alias state → 
    "(" <> showState state <> ") " <> alias

sepFor ∷ Source → String
sepFor = case _ of
  Product _ → ", "
  LeftJoin _ _ → " LEFT JOIN "

showSource ∷ Source → String
showSource = case _ of
  Product t → showSQL t
  LeftJoin t e → showSQL t <> " ON (" <> showExpr e <> ")"

showAliasedCol ∷ Tuple Alias (Exists Expr) → String
showAliasedCol (Tuple alias ee) = runExists showExpr ee <> " AS " <> alias
