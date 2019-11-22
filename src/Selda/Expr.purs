module Selda.Expr where

import Prelude

import Control.Monad.State (State, get, put, runState)
import Data.Array as Array
import Data.Exists (Exists, runExists)
import Data.Leibniz (type (~))
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe)
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
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
  | EForeign Foreign

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

-- | Keeps a list of parameters that will be passed to the backend-specific
-- | query execution (which takes SQL query as String with placeholders $<int>
-- | and an array with parameters that correspond to these placeholders)
type QueryParams =
  { invertedParams ∷ List Foreign 
  , nextIndex ∷ Int
  }

-- | State monad for: (Query AST) → (Query String with placeholders, Parameters)
type ShowM = State QueryParams String

runShowM ∷ Int → ShowM → Tuple String QueryParams
runShowM firstIndex m = runState m
  { invertedParams: mempty, nextIndex: firstIndex }

showM
  ∷ Int
  → ShowM
  → { params ∷ Array Foreign, nextIndex ∷ Int, strQuery ∷ String }
showM i m = { params, nextIndex, strQuery }
  where
    (Tuple strQuery { invertedParams, nextIndex }) = runShowM i m
    params = Array.fromFoldable $ List.reverse invertedParams

showForeign ∷ Foreign → ShowM
showForeign x = do
  s ← get
  put $ s { nextIndex = 1 + s.nextIndex, invertedParams = x : s.invertedParams }
  pure $ "$" <> show s.nextIndex

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

showExpr ∷ ∀ a. Expr a → ShowM
showExpr = case _ of
  EColumn col → pure $ showColumn col
  ELit lit → pure $ showLiteral lit
  EBinOp e → runExists showBinExp e
  EUnOp e → runExists showUnExp e
  EFn fn → runExists showFn fn
  EInArray e → runExists showInArray e
  EForeign x → showForeign x

showBinExp ∷ ∀ o i. BinExp o i → ShowM
showBinExp (BinExp op e1 e2) = do
  s1 ← showExpr e1
  s2 ← showExpr e2
  pure $ "(" <> s1 <> showBinOp op <> s2 <> ")"

showUnExp ∷ ∀ o i. UnExp o i → ShowM
showUnExp (UnExp op e) = do
  let
    ret s = "(" <> s <> ")"
    matchOp s = case op of
      IsNull _ → s <> " IS NOT NULL"
      Not _ _ → "NOT " <> s
  ret <$> matchOp <$> showExpr e

showFn ∷ ∀ o i. Fn o i → ShowM
showFn fn = 
  let ret op e = (\s → op <> "(" <> s <> ")") <$> showExpr e in
  case fn of
    FnMax e _ → ret "max" e
    FnCount e _ → ret "count" e
    FnSum e _ → ret "sum" e

showInArray ∷ ∀ o i. InArray o i → ShowM
showInArray (InArray x xs _) = do
  s ← showExpr x
  ss ← traverse showExpr xs
  pure $ "(" <> s <> " IN (" <> joinWith ", " ss <> "))"
