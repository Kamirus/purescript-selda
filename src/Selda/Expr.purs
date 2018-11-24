module Selda.Expr where

import Prelude

import Data.Exists (Exists, runExists)
import Data.Leibniz (type (~))
import Prim.RowList (kind RowList)
import Selda.Table (Column, showColumn)

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
  | EFn (Fn o)

data BinExp o i = BinExp (BinOp i o) (Expr i) (Expr i)

data Fn o
  = FnMax (Expr o)
  | FnCount (Exists Expr) (String ~ o)

showLiteral ∷ ∀ a. Literal a → String
showLiteral = case _ of
  LBoolean b _ → show b
  LString s _ → "'" <> s <> "'"
  LInt i _ → show i

showBinOp ∷ ∀ i o. BinOp i o → String
showBinOp = case _ of
  Or _ _ → " || "
  Gt _ → " > "
  Eq _ → " = "

showExpr ∷ ∀ a. Expr a → String
showExpr = case _ of
  EColumn col → showColumn col
  ELit lit → showLiteral lit
  EBinOp e → runExists showBinExp e
  EFn fn → showFn fn

showBinExp ∷ ∀ o i. BinExp o i → String
showBinExp (BinExp op e1 e2) = "(" <> showExpr e1 <> showBinOp op <> showExpr e2 <> ")"

showFn ∷ ∀ o. Fn o → String
showFn = case _ of
  FnMax e → "max(" <> showExpr e <> ")"
  FnCount ee _ → "count(" <> runExists showExpr ee <> ")"
