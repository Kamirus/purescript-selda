module Selda.Col
  ( Col(..)
  , class Lit
  , lit
  , class ToCols
  , toCols
  , (.==), expEq
  , (.>), expGt
  -- , (.<)
  -- , (.>=)
  -- , (.<=)
  -- , (.&&)
  , (.||), expOr
  ) where

import Prelude

import Data.Exists (mkExists)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as R
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Selda.Expr (BinExp(..), BinOp(..), Expr(..), Literal(..))
import Selda.Table (Column)
import Type.Proxy (Proxy)
import Type.Row (RLProxy(..))

newtype Col s a = Col (Expr a)
derive instance newtypeCol ∷ Newtype (Col s a) _
instance showCol ∷ Show (Col s a) where show = unwrap >>> show

class Lit a where
  lit ∷ ∀ s. a → Col s a
instance litBoolean ∷ Lit Boolean where lit x = Col $ ELit $ LBoolean x identity
instance litString ∷ Lit String where lit x = Col $ ELit $ LString x identity
instance litInt ∷ Lit Int where lit x = Col $ ELit $ LInt x identity

-- { name ∷ Column String, id ∷ Column Int } → { name ∷ Col s String, id ∷ Col s Int }
class ToCols s (i ∷ # Type) (ri ∷ RowList) (o ∷ # Type) | s i ri → o where
  toCols ∷ Proxy s → Record i → RLProxy ri → Record o

instance toColsNil ∷ ToCols s i RL.Nil () where
  toCols _ _ _ = {}

instance toColsCons
    ∷ ( IsSymbol sym
      , R.Lacks sym o'
      , R.Cons sym (Col s t) o' o
      , R.Cons sym (Column t) i' i
      , ToCols s i tail o'
      )
    ⇒ ToCols s i (RL.Cons sym (Column t) tail) o
  where
  toCols s i _ = do
    let
      _sym = (SProxy ∷ SProxy sym)
      col = Record.get _sym i
      res' = toCols s i (RLProxy ∷ RLProxy tail)
    Record.insert _sym (Col $ EColumn col) res'

expOr ∷ ∀ s. Col s Boolean → Col s Boolean → Col s Boolean
expOr = binOp (Or identity identity)

expGt ∷ ∀ s a. Col s a → Col s a → Col s Boolean
expGt = binOp (Gt identity)

expEq ∷ ∀ s a. Col s a → Col s a → Col s Boolean
expEq = binOp (Eq identity)

binOp ∷ ∀ s o i. BinOp i o -> Col s i -> Col s i -> Col s o
binOp op (Col e1) (Col e2) = Col $ EBinOp $ mkExists $ BinExp op e1 e2

-- instance colHeytingAlgebra ∷ HeytingAlgebra (Col s Boolean) where

-- infixl 4 `like`
infixl 4 expEq as .==
infixl 4 expGt as .>
-- infixl 4 expLt as .<
-- infixl 4 expGe as .>=
-- infixl 4 expLe as .<=
-- infixr 3 expAnd as .&&
infixr 2 expOr as .||
