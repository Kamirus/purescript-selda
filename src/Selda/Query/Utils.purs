module Selda.Query.Utils where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
import Heterogeneous.Folding (class Folding, class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row as R
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text, Beside)
import Record as Record
import Selda.Col (class ToCols, Col, toCols)
import Selda.Table (class TableColumns, Table(..), tableColumns)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)

type App a b = a b
infixr 0 type App as :=>

class MappingRL f a b | f a → b

class MapRL f (i ∷ RowList) (o ∷ RowList) | f i → o
instance mapRLNil ∷ MapRL f RL.Nil RL.Nil
instance mapRLCons
  ∷ ( MappingRL f a a'
    , MapRL f tail tail'
    )
  ⇒ MapRL f (RL.Cons sym a tail) (RL.Cons sym a' tail')

class MapR f (i ∷ #Type) (o ∷ #Type) | f i → o
instance mapR
  ∷ ( RL.RowToList i il
    , MapRL f il ol
    , ListToRow ol o
    )
  ⇒ MapR f i o

data UnCol_
instance unColRL ∷ UnCol a b ⇒ MappingRL UnCol_ a b

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
  ∷ Fail (Text sym <:> Text " is not Col or the scope 's' is wrong")
  ⇒ ValidateSInCols s (RL.Cons sym col tail)

class UnCol i o | i → o
instance mapTypeCol ∷ UnCol (Col s a) a

data TupleToRecordFunc = TupleToRecordFunc
instance tupToRec
    ∷ ( IsSymbol sym
      , R.Lacks sym r
      , R.Cons sym a r r'
      , UnCol i a
      )
    ⇒ FoldingWithIndex TupleToRecordFunc 
      (SProxy sym) (tup → { | r }) i (Tuple a tup → { | r' })
  where
  foldingWithIndex TupleToRecordFunc sym f _ =
    \(Tuple a tup) → Record.insert (SProxy ∷ SProxy sym) a $ f tup

data RecordToTuple = RecordToTuple
instance rToTuple ∷ Folding RecordToTuple tail a (Tuple a tail) where
  folding _ tail a = Tuple a tail

data RecordToArrayForeign b = RecordToArrayForeign (Proxy b)
instance rToArrForeign
    ∷ ToForeign b a
    ⇒ Folding (RecordToArrayForeign b) (Array Foreign) a (Array Foreign)
  where
  folding (RecordToArrayForeign b) acc a = [toForeign b a] <> acc

class ToForeign b a where
  toForeign ∷ Proxy b → a → Foreign

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
class TableToColsWithoutAlias s r o | r → o where
  tableToColsWithoutAlias ∷ Proxy s → Table r → { | o }

instance tableToColsI
    ∷ ( RL.RowToList r rl
      , TableColumns rl i
      , ToCols s i o
      )
    ⇒ TableToColsWithoutAlias s r o
  where
  tableToColsWithoutAlias _ (Table { name }) = recordWithCols
    where
    aliased = { name, alias: "" }
    recordWithColumns = tableColumns aliased (RLProxy ∷ RLProxy rl)
    recordWithCols = toCols (Proxy ∷ Proxy s) recordWithColumns

infixl 4 type Beside as <:>
