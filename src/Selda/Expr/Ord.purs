module Selda.Expr.Ord where

import Prelude

import Selda.Aggr (Aggr(..))
import Selda.Col (Col, binOp)
import Selda.Expr (BinOp(..))

infix 4 exprEq as .==
infix 4 exprNeq as ./=
infix 4 exprGt as .>
infix 4 exprLt as .<
infix 4 exprGe as .>=
infix 4 exprLe as .<=

exprNeq
  ∷ ∀ expr a
  . ExprEq expr ⇒ HeytingAlgebra (expr Boolean)
  ⇒ expr a → expr a → expr Boolean
exprNeq a b = not $ a .== b

-- | Represents types that can model expressions with equality.
class ExprEq expr where
  exprEq ∷ ∀ a. expr a → expr a → expr Boolean

instance exprEqCol ∷ ExprEq (Col s) where
  exprEq = binOp (Eq identity)

instance exprEqAggr ∷ ExprEq (Aggr s) where
  exprEq (Aggr a) (Aggr b) = Aggr $ a .== b

-- | Represents types that model expressions with ordering.
class ExprEq expr <= ExprOrd expr where
  exprGt ∷ ∀ a. expr a → expr a → expr Boolean
  exprGe ∷ ∀ a. expr a → expr a → expr Boolean
  exprLt ∷ ∀ a. expr a → expr a → expr Boolean
  exprLe ∷ ∀ a. expr a → expr a → expr Boolean

instance exprOrdCol ∷ ExprOrd (Col s) where
  exprGt = binOp (Gt identity)
  exprGe = binOp (Ge identity)
  exprLt = binOp (Lt identity)
  exprLe = binOp (Le identity)

instance exprOrdAggr ∷ ExprOrd (Aggr s) where
  exprGt (Aggr a) (Aggr b) = Aggr $ exprGt a b
  exprGe (Aggr a) (Aggr b) = Aggr $ exprGe a b
  exprLt (Aggr a) (Aggr b) = Aggr $ exprLt a b
  exprLe (Aggr a) (Aggr b) = Aggr $ exprLe a b
