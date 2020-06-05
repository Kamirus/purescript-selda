module Selda
  ( module Query.Type
  , module Col
  , module ShowStatement
  , module Query
  , module Table
  , (.==), expEq
  , (.>), expGt
  , (.<), expLt
  , (.>=), expGe
  , (.<=), expLe
  , (.&&), expAnd
  , (.||), expOr
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
import Selda.Col (Col(..), lit, class Lit) as Col
import Selda.Col (Col(..))
import Selda.Expr (BinExp(..), BinOp(..), Expr(..), Fn(..), InArray(..), UnExp(..), UnOp(..))
import Selda.Query (crossJoin, crossJoin_, innerJoin, innerJoin_, restrict, notNull, union, unionAll, intersect, except, leftJoin, leftJoin_, distinct, aggregate, groupBy, groupBy', selectFrom, selectFrom_, limit, orderBy) as Query
import Selda.Query.ShowStatement (showQuery, showDeleteFrom, showUpdate) as ShowStatement
import Selda.Query.Type (Order(..))
import Selda.Query.Type (Query(..), FullQuery(..)) as Query.Type
import Selda.Table (Table(..)) as Table

-- | Deprecated: use `&&`
expAnd ∷ ∀ s. Col s Boolean → Col s Boolean → Col s Boolean
expAnd = (&&)

-- | Deprecated: use `||`
expOr ∷ ∀ s. Col s Boolean → Col s Boolean → Col s Boolean
expOr = (||)

expGt ∷ ∀ s a. Col s a → Col s a → Col s Boolean
expGt = binOp (Gt identity)

expGe ∷ ∀ s a. Col s a → Col s a → Col s Boolean
expGe = binOp (Ge identity)

expLt ∷ ∀ s a. Col s a → Col s a → Col s Boolean
expLt = binOp (Lt identity)

expLe ∷ ∀ s a. Col s a → Col s a → Col s Boolean
expLe = binOp (Le identity)

expEq ∷ ∀ s a. Col s a → Col s a → Col s Boolean
expEq = binOp (Eq identity)

binOp ∷ ∀ s o i. BinOp i o → Col s i → Col s i → Col s o
binOp op (Col e1) (Col e2) = Col $ EBinOp $ mkExists $ BinExp op e1 e2

-- | returns String because the value might not fit in the underlying js float
count ∷ ∀ s a. Col s a → Aggr s Int
count (Col e) = Aggr $ Col $ EFn $ mkExists $ FnCount e identity

-- | returns Maybe in case of empty set aggregation
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

asc ∷ Order
asc = Asc

desc ∷ Order
desc = Desc

-- infixl 4 `like`
infixl 4 expEq as .==
infixl 4 expGt as .>
infixl 4 expLt as .<
infixl 4 expGe as .>=
infixl 4 expLe as .<=
-- | Deprecated: use `&&`
infixr 3 expAnd as .&&
-- | Deprecated: use `||`
infixr 2 expOr as .||
