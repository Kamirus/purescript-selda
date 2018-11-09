module Expr where

import Prelude

import Data.Exists (Exists, mkExists, runExists)
import Data.Leibniz (type (~), coerce, coerceSymm)
import Prim.RowList (kind RowList)

newtype Table ( r ∷ # Type ) = Table { name ∷ String }

type AliasedTable = { name ∷ String, alias ∷ String }

newtype Col a = Col { table ∷ AliasedTable, name ∷ String }
showCol ∷ ∀ a. Col a → String
showCol (Col { table, name }) = table.alias <> "." <> name

{-
typeclass
leibniz + exists
polymor variant
-}

data Lit a
  = LBoolean Boolean (Boolean ~ a)
  | LString String (String ~ a)
  | LInt Int (Int ~ a)

data BinOp i o
  = Or (Boolean ~ i) (Boolean ~ o)
  | Gt (Boolean ~ o)
  -- | Eq (Boolean ~ o)

data Expr o
  = ECol (Col o)
  | ELit (Lit o)
  | BinaryOp (Exists (BinOpExp o)) -- | EBinOp (BinOp o i) (Expr i i) (Expr i i)

data BinOpExp o i = BinOpExp (BinOp i o) (Expr i) (Expr i)

showLit ∷ ∀ a. Lit a → String
showLit = case _ of
  LBoolean b _ → show b
  LString s _ → show s
  LInt i _ → show i

showExpr ∷ ∀ o. Expr o → String
showExpr = case _ of
  ECol col → showCol col
  ELit lit → showLit lit
  BinaryOp e → runExists f e
    where
    f ∷ ∀ i. BinOpExp o i → String
    f (BinOpExp (Or _ _) e1 e2) = "(" <> showExpr e1 <> " || " <> showExpr e2 <> ")"
    f (BinOpExp (Gt _) e1 e2) = "(" <> showExpr e1 <> " > " <> showExpr e2 <> ")"

-- bar ∷ Lit Boolean
-- bar = LBoolean true identity

-- fromExpr ∷ Expr o → o
-- fromExpr 

-- foo ∷ ∀ o. Expr o → Boolean
-- foo (ELit (LBoolean b f)) = b
-- foo (BinaryOp e) = runExists f e
--   where
--   f ∷ ∀ i. BinOpExp o i → Boolean
--   f (BinOpExp (Or fi fo) e1 e2) = foo e1 || foo e2 -- foo e1 <> " + " <> foo e2
--   -- f (BinOpExp op e1 e2) = foo e1 <> " + " <> foo e2

-- eval ∷ ∀ a. Lit a → a
-- eval (LBoolean i f) = coerce f i

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
-- instance toExpReprString ∷ ExpRepr r ⇒ ToExpRepr String r String where toExpRepr = fromString

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

-- expEq = binOp eqq
-- expGt = binOp gt

expGt ∷ ∀ a. Expr a → Expr a → Expr Boolean
expGt e1 e2 = BinaryOp $ mkExists e
  where
  e ∷ BinOpExp Boolean a
  e = BinOpExp (Gt identity) e1 e2
-- expGt = binOp gt

-- infix 8 expEq as .==
infix 8 expGt as .>

