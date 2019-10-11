module Selda.Expr where

import Prelude

import Data.Exists (Exists, runExists)
import Data.Leibniz (type (~))
import Data.Maybe (Maybe)
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Prim.RowList (kind RowList)
import Selda.Table (Column, showColumn)

data Literal a
  = LBoolean Boolean (Boolean ~ a)
  | LString String (String ~ a)
  | LInt Int (Int ~ a)
  | LNull (Exists (None a))
  | LJust (Exists (Some a))
  | Any String

data Some a b = Some (Literal b) (Maybe b ~ a)

data None a b = None (Maybe b ~ a)

data BinOp i o
  = Or (Boolean ~ i) (Boolean ~ o)
  | Gt (Boolean ~ o)
  | Eq (Boolean ~ o)

data UnOp i o
  = IsNull (Boolean ~ o)
  | Not (Boolean ~ i) (Boolean ~ o)

data Expr o
  = EColumn (Column o)
  | ELit (Literal o)
  | EBinOp (Exists (BinExp o))
  | EUnOp (Exists (UnExp o))
  | EFn (Exists (Fn o))
  | EInArray (Exists (InArray o)) 

data BinExp o i = BinExp (BinOp i o) (Expr i) (Expr i)

data UnExp o i = UnExp (UnOp i o) (Expr i)

data Fn o i
  = FnMax (Expr i) (Maybe i ~ o)
  | FnCount (Expr i) (String ~ o)
  | FnSum (Expr i) (Maybe String ~ o)

data InArray o i = InArray (Expr i) (Array (Expr i)) (Boolean ~ o)

primPGEscape ∷ String → String
primPGEscape = toCharArray >>> (_ >>= escape) >>> fromCharArray
  where
  escape ∷ Char → Array Char
  escape c = case c of
    '\'' → [c, c]
    '\\' → [c, c]
    _ → pure c

showLiteral ∷ ∀ a. Literal a → String
showLiteral = case _ of
  LBoolean b _ → show b
  LString s _ → "E'" <> primPGEscape s <> "'"
  LInt i _ → show i
  LNull _ → "null"
  LJust x → runExists (\(Some l _) → showLiteral l) x
  Any s → "E'" <> primPGEscape s <> "'"

showBinOp ∷ ∀ i o. BinOp i o → String
showBinOp = case _ of
  Or _ _ → " or "
  Gt _ → " > "
  Eq _ → " = "

showExpr ∷ ∀ a. Expr a → String
showExpr = case _ of
  EColumn col → showColumn col
  ELit lit → showLiteral lit
  EBinOp e → runExists showBinExp e
  EUnOp e → runExists showUnExp e
  EFn fn → runExists showFn fn
  EInArray e → runExists showInArray e

showBinExp ∷ ∀ o i. BinExp o i → String
showBinExp (BinExp op e1 e2) = "(" <> showExpr e1 <> showBinOp op <> showExpr e2 <> ")"

showUnExp ∷ ∀ o i. UnExp o i → String
showUnExp (UnExp op e) = (\s → "(" <> s <> ")") $
  case op of 
    IsNull _ → showExpr e <> " IS NOT NULL"
    Not _ _ → "NOT " <> showExpr e

showFn ∷ ∀ o i. Fn o i → String
showFn = case _ of
  FnMax e _ → "max(" <> showExpr e <> ")"
  FnCount e _ → "count(" <> showExpr e <> ")"
  FnSum e _ → "sum(" <> showExpr e <> ")"

showInArray ∷ ∀ o i. InArray o i → String
showInArray (InArray x xs _) = "(" <> showExpr x <> " IN " <> "(" <> l <> "))"
  where
    l = joinWith ", " $ map showExpr xs
