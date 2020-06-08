module Selda
  ( module Query.Type
  , module Col
  , module ShowStatement
  , module Query
  , module Table
  , S
  , (.==), expEq
  , (.>), expGt
  , (.<), expLt
  , (.>=), expGe
  , (.<=), expLe
  , (.&&), expAnd
  , (.||), expOr
  , class CoerceBinExpr, onCoerceBinExpr
  , count
  , max_
  , sum_
  , not_
  , inArray
  , isNull
  , asc, desc
  ) where

import Prelude

import Data.Exists (mkExists)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Selda.Aggr (Aggr(..))
import Selda.Col (Col(..))
import Selda.Col (Col(..), lit, class Lit) as Col
import Selda.Expr (Expr(..), Fn(..), InArray(..), UnExp(..), UnOp(..))
import Selda.Expr.Ord (class ExprOrd)
import Selda.Expr.Ord as Ord
import Selda.Query (crossJoin, crossJoin_, innerJoin, innerJoin_, restrict, notNull, union, unionAll, intersect, except, leftJoin, leftJoin_, distinct, aggregate, groupBy, groupBy', selectFrom, selectFrom_, limit, orderBy) as Query
import Selda.Query.ShowStatement (showQuery, showDeleteFrom, showUpdate) as ShowStatement
import Selda.Query.Type (Order(..))
import Selda.Query.Type (Query(..), FullQuery(..)) as Query.Type
import Selda.Table (Table(..)) as Table

-- | Top-level scope of a query
type S = Unit

asc ∷ Order
asc = Asc

desc ∷ Order
desc = Desc

-- infixl 4 `like`
infix 4 expEq as .==
infix 4 expNeq as ./=
infix 4 expGt as .>
infix 4 expLt as .<
infix 4 expGe as .>=
infix 4 expLe as .<=
infixr 3 expAnd as .&&
infixr 2 expOr as .||

-- | returns String because the value might not fit in the underlying js float
count ∷ ∀ s a. Col s a → Aggr s Int
count (Col e) = Aggr $ Col $ EFn $ mkExists $ FnCount e identity

-- | returns `Nothing` in case of empty set aggregation
max_ ∷ ∀ s a. Col s a → Aggr s (Maybe a)
max_ (Col e) = Aggr $ Col $ EFn $ mkExists $ FnMax e identity

sum_ ∷ ∀ s a. Col s a → Aggr s (Maybe Int)
sum_ (Col e) = Aggr $ Col $ EFn $ mkExists $ FnSum e identity

not_ ∷ ∀ s. Col s Boolean → Col s Boolean
not_ (Col e) = Col $ EUnOp $ mkExists $ UnExp (Not identity identity) e

inArray ∷ ∀ s a. Col s a → Array (Col s a) → Col s Boolean
inArray (Col e) cols = Col $ EInArray $ mkExists $ InArray e exprs identity
  where exprs = map unwrap cols

isNull ∷ ∀ s a. Col s (Maybe a) → Col s Boolean
isNull (Col e) = Col $ EUnOp $ mkExists $ UnExp (IsNull identity) e

-- | `(&&)` on `Col` or `Aggr` expressions. Possible monomorphic types:
-- |
-- | `Col s Bool → Col s Bool → Col s Bool`
-- |
-- | `Col s Bool → Aggr s Bool → Aggr s Bool`
-- |
-- | `Aggr s Bool → Col s Bool → Aggr s Bool`
-- |
-- | `Aggr s Bool → Aggr s Bool → Aggr s Bool`
-- |
expAnd
  ∷ ∀ col1 col2 col s
  . CoerceBinExpr col1 col2 col
  ⇒ HeytingAlgebra (col s Boolean)
  ⇒ col1 s Boolean → col2 s Boolean → col s Boolean
expAnd = onCoerceBinExpr (&&)

expOr
  ∷ ∀ col1 col2 col s
  . CoerceBinExpr col1 col2 col
  ⇒ HeytingAlgebra (col s Boolean)
  ⇒ col1 s Boolean → col2 s Boolean → col s Boolean
expOr = onCoerceBinExpr (||)

type CoercedOrderingExpr = 
  ∀ col1 col2 col s a
  . CoerceBinExpr col1 col2 col
  ⇒ ExprOrd (col s)
  ⇒ col1 s a → col2 s a → col s Boolean

expGt ∷ CoercedOrderingExpr
expGt = onCoerceBinExpr Ord.(.>)

expGe ∷ CoercedOrderingExpr
expGe = onCoerceBinExpr Ord.(.>=)

expLt ∷ CoercedOrderingExpr
expLt = onCoerceBinExpr Ord.(.<)

expLe ∷ CoercedOrderingExpr
expLe = onCoerceBinExpr Ord.(.<=)

expEq ∷ CoercedOrderingExpr
expEq = onCoerceBinExpr Ord.(.==)

expNeq
  ∷ ∀ col1 col2 col s a
  . CoerceBinExpr col1 col2 col
  ⇒ ExprOrd (col s)
  ⇒ HeytingAlgebra (col s Boolean)
  ⇒ col1 s a → col2 s a → col s Boolean
expNeq a b = not $ a .== b

-- | Allows writing binary expression with different types `e1` and `e2`
-- | coerible to a common expression type `e`.
-- |
-- | Used to mix `Aggr` and `Col` types in expressions.
-- |
-- | Implicit coercion is desired here, because explicit use of `Aggr`
-- | is discouraged as it can break safety when used in other contexts
-- | (like returning a column that is not in the `GROUP BY` clause)
class CoerceBinExpr e1 e2 e | e1 e2 → e where
  onCoerceBinExpr
    ∷ ∀ s a r
    . (e s a → e s a → r)
    → e1 s a
    → e2 s a
    → r

instance cbeColToCol ∷ CoerceBinExpr Col Col Col where
  onCoerceBinExpr f a b = f a b
instance cbeColToAggrL ∷ CoerceBinExpr Col Aggr Aggr where
  onCoerceBinExpr f a b = f (Aggr a) b
instance cbeColToAggrR ∷ CoerceBinExpr Aggr Col Aggr where
  onCoerceBinExpr f a b = f a (Aggr b)
instance cbeAggrToAggr ∷ CoerceBinExpr Aggr Aggr Aggr where
  onCoerceBinExpr f a b = f a b
