module Selda.PG.Utils where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Heterogeneous.Folding (class Folding, class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row as R
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Selda.Col (Col)
import Type.Proxy (Proxy)

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
      , HFoldlWithIndex TupleToRecordFunc (Unit → {}) { | i } (tup → { | o })
      )
    ⇒ ColsToPGHandler s i tup o
  where
  colsToPGHandler _ i = hfoldlWithIndex TupleToRecordFunc f i
    where f = (const {} ∷ Unit → {})

class ValidateSInCols s (il ∷ RowList) (ol ∷ RowList) | s il → ol
instance rLUnColNil ∷ ValidateSInCols s RL.Nil RL.Nil
else instance rLUnColCons
  ∷ ValidateSInCols s tail tail'
  ⇒ ValidateSInCols s (RL.Cons sym (Col s t) tail) (RL.Cons sym t tail')

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

class TupleToRecord tup r | r → tup where
  tupleToRecord ∷ { | r } → (tup → { | r })

instance tupTR
    ∷ HFoldlWithIndex TupleToRecordFunc (Unit → {}) { | r } (tup → { | r })
    ⇒ TupleToRecord tup r
  where
  tupleToRecord r = hfoldlWithIndex TupleToRecordFunc (const {} ∷ Unit → {}) r

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
