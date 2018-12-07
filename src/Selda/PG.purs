module Selda.PG
  ( withPG
  , insert_
  , TupleToRecordFunc
  , RecordToTuple
  , RecordLength
  , class ChangeType
  -- , class BuildPGHandler
  , class ValidateSInCols
  , class TupleRev, tupleRev
  -- , buildPGHandler
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
import Database.PostgreSQL (class FromSQLRow, class ToSQLRow, PoolConfiguration)
import Database.PostgreSQL as PG
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Heterogeneous.Folding (class Folding, class FoldingWithIndex, class HFoldl, class HFoldlWithIndex, hfoldl, hfoldlWithIndex)
import Prim.Row as R
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Selda.Col (class GetCols, Col, getCols)
import Selda.Expr (Expr, showExpr)
import Selda.Query.Type (Source(..), Query, SQL(..), GenState, runQuery)
import Selda.Table (class TableColumnNames, Alias, Table(..), tableColumnNames)
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
      -- , BuildPGHandler ol tup o
      , HFoldlWithIndex TupleToRecordFunc (Unit → {}) { | i } (tup → { | o })
      )
    ⇒ ColsToPGHandler s i tup o
  where
  colsToPGHandler _ i = hfoldlWithIndex TupleToRecordFunc f i
    where f = (const {} ∷ Unit → {})
  -- colsToPGHandler _ i = buildPGHandler (RLProxy ∷ RLProxy ol)

class ValidateSInCols s (il ∷ RowList) (ol ∷ RowList) | s il → ol
instance rLUnColNil ∷ ValidateSInCols s RL.Nil RL.Nil
else instance rLUnColCons
  ∷ ValidateSInCols s tail tail'
  ⇒ ValidateSInCols s (RL.Cons sym (Col s t) tail) (RL.Cons sym t tail')

-- class BuildPGHandler (il ∷ RowList) tup (o ∷ # Type) | il → tup o where
--   buildPGHandler
--     ∷ RLProxy il
--     → (tup → Record o)

-- instance buildPGHandlerHead
--     ∷ ( IsSymbol sym
--       , R.Lacks sym ()
--       , R.Cons sym t () o
--       )
--     ⇒ BuildPGHandler (RL.Cons sym t RL.Nil) (Tuple t Unit) o
--   where
--   buildPGHandler _ =
--     \(Tuple t _) → Record.insert (SProxy ∷ SProxy sym) t {}
-- else instance buildPGHandlerCons
--     ∷ ( IsSymbol sym
--       , R.Lacks sym o'
--       , R.Cons sym t o' o
--       , BuildPGHandler tail tup o'
--       )
--     ⇒ BuildPGHandler (RL.Cons sym t tail) (Tuple t tup) o
--   where
--   buildPGHandler _ =
--     let f' = buildPGHandler (RLProxy ∷ RLProxy tail) in
--     \(Tuple t tup) → Record.insert (SProxy ∷ SProxy sym) t $ f' tup

class ChangeType i o | i → o
instance mapTypeCol ∷ ChangeType (Col s a) a
else instance mapType ∷ ChangeType a a

data TupleToRecordFunc = TupleToRecordFunc
instance tupToRec
    ∷ ( IsSymbol sym
      , R.Lacks sym r
      , R.Cons sym a r r'
      , ChangeType i a
      )
    ⇒ FoldingWithIndex TupleToRecordFunc 
      (SProxy sym) (tup → { | r }) i (Tuple a tup → { | r' })
  where
  foldingWithIndex TupleToRecordFunc sym f _ =
    \(Tuple a tup) → Record.insert (SProxy ∷ SProxy sym) a $ f tup

data RecordToTuple = RecordToTuple
instance rToTuple ∷ Folding RecordToTuple tail a (Tuple a tail) where
  folding _ tail a = Tuple a tail

class TupleRev t1 acc t2 | t1 acc → t2 where
  tupleRev ∷ t1 → acc → t2
instance tuplerevh ∷ TupleRev Unit acc acc where
  tupleRev _ t = t
else instance tuplerevc
    ∷ TupleRev b (Tuple a acc) res
    ⇒ TupleRev (Tuple a b) acc res
  where
  tupleRev (Tuple a b) acc = tupleRev b (Tuple a acc)

data RecordLength = RecordLength
instance rlen ∷ Folding RecordLength Int a Int where
  folding _ acc _ = acc + 1

insert_
  ∷ ∀ r rl tup
  -- HFoldlWithIndex TupleToRecordFunc (Unit → {}) { | r } (tup → { | o })
  -- ⇒ BuildPGHandler rl tup o
  . FromSQLRow tup
  ⇒ ToSQLRow tup
  ⇒ Show tup
  ⇒ RL.RowToList r rl
  ⇒ TableColumnNames rl
  ⇒ HFoldl RecordToTuple Unit { | r } tup
  ⇒ HFoldl RecordLength Int { | r } Int
  ⇒ PoolConfiguration → Table r → { | r } → Aff Unit
insert_ dbconfig (Table { name }) x = do
  -- case _ of
  -- [] → pure []
  -- xs → do
  let
    cols = joinWith ", " $ tableColumnNames (RLProxy ∷ RLProxy rl)
    xTup = hfoldl RecordToTuple unit x
    xLen = hfoldl RecordLength 0 x
    placeholders = Array.range 1 xLen # map (\i → "$" <> show i) # joinWith ", "
    q_str = 
      "INSERT INTO " <> name <> " (" <> cols <> ") " 
        <> "VALUES " <> "(" <> placeholders <> ") "
        <> "RETURNING *"
  -- liftEffect $ log q_str
  -- liftEffect $ log $ show xTup
  pool ← PG.newPool dbconfig
  PG.withConnection pool \conn → do
    PG.execute conn (PG.Query q_str) xTup
    -- rows ← PG.query conn (PG.Query q_str) xTup
    -- pure $ map (hfoldlWithIndex TupleToRecordFunc (const {} ∷ Unit → {}) x) rows
    -- pure $ map (buildPGHandler (RLProxy ∷ RLProxy rl)) rows

withPG
  ∷ ∀ o i tup s
  . ColsToPGHandler s i tup o
  ⇒ GetCols i
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
