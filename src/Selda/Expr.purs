module Selda.Expr where

import Prelude

import Data.Exists (Exists, runExists)
import Data.Leibniz (type (~))
import Prim.RowList (kind RowList)
import Selda.Table (Column)

data Literal a
  = LBoolean Boolean (Boolean ~ a)
  | LString String (String ~ a)
  | LInt Int (Int ~ a)

data BinOp i o
  = Or (Boolean ~ i) (Boolean ~ o)
  | Gt (Boolean ~ o)
  | Eq (Boolean ~ o)

data Expr o
  = EColumn (Column o)
  | ELit (Literal o)
  | EBinOp (Exists (BinExp o))

data BinExp o i = BinExp (BinOp i o) (Expr i) (Expr i)

instance showLiteral ∷ Show (Literal a) where
  show = case _ of
    LBoolean b _ → show b
    LString s _ → "'" <> show s <> "'"
    LInt i _ → show i

instance showBinOp ∷ Show (BinOp i o) where
  show = case _ of
    Or _ _ → " || "
    Gt _ → " > "
    Eq _ → " = "

instance showExpr ∷ Show (Expr a) where
  show = case _ of
    EColumn col → show col
    ELit lit → show lit
    EBinOp e → runExists show e

instance showBinExp ∷ Show (BinExp o i) where
  show (BinExp op e1 e2) = "(" <> show e1 <> show op <> show e2 <> ")"
