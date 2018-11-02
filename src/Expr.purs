module Expr where

import Prim.RowList (kind RowList)
import Types (Col)

class ExpRepr r where
  fromCol ∷ ∀ a. Col a → r a
  fromInt ∷ Int → r Int
  fromBoolean ∷ Boolean → r Boolean
  fromString ∷ String → r String

class ExpOps expr where
  eqq ∷ ∀ a. expr a → expr a → expr Boolean
  gt ∷ ∀ a. expr a → expr a → expr Boolean

class ToExpRepr a r b | a → r b where
  toExpRepr ∷ a → r b

instance toExpReprCol ∷ ExpRepr r ⇒ ToExpRepr (Col a) r a where toExpRepr = fromCol
instance toExpReprInt ∷ ExpRepr r ⇒ ToExpRepr Int r Int where toExpRepr = fromInt
instance toExpReprBoolean ∷ ExpRepr r ⇒ ToExpRepr Boolean r Boolean where toExpRepr = fromBoolean
instance toExpReprString ∷ ExpRepr r ⇒ ToExpRepr String r String where toExpRepr = fromString

binOp
  ∷ ∀ expr x a b y
  . ExpOps expr
  ⇒ ToExpRepr a expr x
  ⇒ ToExpRepr b expr x
  ⇒ (expr x → expr x → expr y)
  → a
  → b
  → expr y
binOp f e1 e2 = f (toExpRepr e1) (toExpRepr e2)

expEq = binOp eqq
expGt = binOp gt

infix 8 expEq as .==
infix 8 expGt as .>

