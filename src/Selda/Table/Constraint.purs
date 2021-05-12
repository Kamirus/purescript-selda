module Selda.Table.Constraint where

import Prim.Row as R
import Prim.RowList as RL
import Type.RowList (class ListToRow, class RowListAppend)

-- | Auto Constraint
foreign import data Auto ∷ Type → Type

-- | Default Constraint
foreign import data Default ∷ Type → Type

class EraseConstraint :: forall k1 k2. k1 -> k2 -> Constraint
class EraseConstraint a b | a → b
instance eraseAuto ∷ EraseConstraint (Auto col) col
else instance eraseDefault ∷ EraseConstraint (Default col) col
else instance nothingToErase ∷ EraseConstraint col col

class MaxColumnsToInsert :: Row Type -> RL.RowList Type -> Constraint
class MaxColumnsToInsert t maxCols | t → maxCols
instance maxColumnsToInsert
  ∷ ( RL.RowToList t tl
    , FilterOutConstraintColumns tl simpleCols
    , FilterDefaultColumns tl defaultCols
    , RowListAppend simpleCols defaultCols maxCols
    )
  ⇒ MaxColumnsToInsert t maxCols

class MinColumnsToInsert :: Row Type -> RL.RowList Type -> Constraint
class MinColumnsToInsert t minCols | t → minCols
instance minColumnsToInsert
  ∷ ( RL.RowToList t tl
    , FilterOutConstraintColumns tl minCols
    )
  ⇒ MinColumnsToInsert t minCols

-- | Removes `Auto` and `Default` columns from `i`
class FilterOutConstraintColumns :: RL.RowList Type -> RL.RowList Type -> Constraint
class FilterOutConstraintColumns i o | i → o
instance filterOutConstraintColumnsNil ∷ FilterOutConstraintColumns RL.Nil RL.Nil
else instance filterOutConstraintColumnsAuto
  ∷ FilterOutConstraintColumns tail rl
  ⇒ FilterOutConstraintColumns (RL.Cons sym (Auto t) tail) rl
else instance filterOutConstraintColumnsDefault
  ∷ FilterOutConstraintColumns tail rl
  ⇒ FilterOutConstraintColumns (RL.Cons sym (Default t) tail) rl
else instance filterOutConstraintColumnsCons
  ∷ FilterOutConstraintColumns tail rl
  ⇒ FilterOutConstraintColumns (RL.Cons sym t tail) (RL.Cons sym t rl)

-- | Returns only `Default` columns with erased `Default` wrapper
class FilterDefaultColumns :: RL.RowList Type -> RL.RowList Type -> Constraint
class FilterDefaultColumns (i ∷ RL.RowList Type) (o ∷ RL.RowList Type) | i → o
instance filterDefaultColumnsNil ∷ FilterDefaultColumns RL.Nil RL.Nil
else instance filterDefaultColumnsConsDefault
  ∷ FilterDefaultColumns tail rl
  ⇒ FilterDefaultColumns (RL.Cons sym (Default t) tail) (RL.Cons sym t rl)
else instance filterDefaultColumnsSkip
  ∷ FilterDefaultColumns tail rl
  ⇒ FilterDefaultColumns (RL.Cons sym t tail) rl

class IsSubRowList :: RL.RowList Type -> RL.RowList Type -> Constraint
class IsSubRowList lhs rhs
instance isSubRowList
  ∷ ( ListToRow rl1 r1
    , ListToRow rl2 r2
    , R.Union r1 diff r2
    )
  ⇒ IsSubRowList rl1 rl2 

class CanInsertColumnsIntoTable :: RL.RowList Type -> Row Type -> Constraint
class CanInsertColumnsIntoTable cols t
instance canInsertColumnsIntoTable
  ∷ ( MaxColumnsToInsert t maxCols
    , MinColumnsToInsert t minCols
    , IsSubRowList minCols cols
    , IsSubRowList cols maxCols
    )
    ⇒ CanInsertColumnsIntoTable cols t

