module Selda.PG.Utils where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Heterogeneous.Folding (class Folding, class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row as R
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Selda.Col (class ToCols, Col, toCols)
import Selda.Table (class TableColumns, Table(..), tableColumns)
import Type.Proxy (Proxy(..))
import Type.Row (RLProxy(..))
import Prim.TypeError (class Fail, Text, Beside)

-- | For record
-- |   `{ n1 ∷ Col s String, n2 ∷ Col s String, id ∷ Col s Int }`
-- | build function
-- |   \Tuple int (Tuple string1 string2) 
-- |     → { id: int, n1: string1, n2: string2 }
class ColsToPGHandler s i tup o | s i → tup o where
  colsToPGHandler ∷ Proxy s → { | i } → (tup → { | o })
instance colsToPGHandlerI
    ∷ ( RL.RowToList i il
      , ValidateSInCols s il
      , HFoldlWithIndex TupleToRecordFunc (Unit → {}) { | i } (tup → { | o })
      )
    ⇒ ColsToPGHandler s i tup o
  where
  colsToPGHandler _ i = hfoldlWithIndex TupleToRecordFunc f i
    where f = (const {} ∷ Unit → {})

class ValidateSInCols s (il ∷ RowList)
instance rLUnColNil ∷ ValidateSInCols s RL.Nil
else instance rLUnColCons
  ∷ ValidateSInCols s tail
  ⇒ ValidateSInCols s (RL.Cons sym (Col s t) tail)
else instance failValidateSInCols 
  ∷ Fail (Beside (Text sym) (Text " is not Col or the scope 's' is wrong"))
  ⇒ ValidateSInCols s (RL.Cons sym col tail)

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

class MkTupleToRecord tup r | r → tup where
  mkTupleToRecord ∷ { | r } → (tup → { | r })

instance tupTR
    ∷ HFoldlWithIndex TupleToRecordFunc (Unit → {}) { | r } (tup → { | r })
    ⇒ MkTupleToRecord tup r
  where
  mkTupleToRecord r = hfoldlWithIndex TupleToRecordFunc (const {} ∷ Unit → {}) r

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

class RowListLength rl where
  rowListLength ∷ RLProxy rl → Int
instance rowListLengthNil ∷ RowListLength RL.Nil where
  rowListLength _ = 0
else instance rowListLengthCons ∷ RowListLength t ⇒ RowListLength (RL.Cons s a t) where
  rowListLength _ = rowListLength (RLProxy ∷ RLProxy t) + 1

-- | ```purescript
-- | Table ( a1 ∷ A1 , a2 ∷ A2 ... )
-- | →
-- | { a1 ∷ Col s A1, a2 ∷ Col s A2 ... }
-- | ```
class TableToColsWithoutAlias r o | r → o where
  tableToColsWithoutAlias ∷ Table r → { | o }

instance tableToColsI
    ∷ ( RL.RowToList r rl
      , TableColumns rl i
      , ToCols s i o
      )
    ⇒ TableToColsWithoutAlias r o
  where
  tableToColsWithoutAlias (Table { name }) = recordWithCols
    where
    aliased = { name, alias: "" }
    recordWithColumns = tableColumns aliased (RLProxy ∷ RLProxy rl)
    recordWithCols = toCols (Proxy ∷ Proxy s) recordWithColumns
