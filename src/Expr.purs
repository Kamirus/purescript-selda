module Expr where

import Prelude

import Control.Monad.State (class MonadState, State, get, put, state)
import Data.Exists (Exists, mkExists, runExists)
import Data.Leibniz (type (~), coerce, coerceSymm)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row as R
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Type.Row (RLProxy(..))

--- Table

newtype Table ( r ∷ # Type ) = Table { name ∷ String }

type AliasedTable = { name ∷ String, alias ∷ String }

newtype Column a = Column { table ∷ AliasedTable, name ∷ String }

-- Table { name ∷ String, id ∷ Int } → { name ∷ Column String, id ∷ Column Int }
class TableCols (rl ∷ RowList) (r ∷ # Type) | rl → r where
  tableCols ∷ AliasedTable → RLProxy rl → Record r

instance tableColsNil ∷ TableCols RL.Nil () where
  tableCols _ _ = {}

instance tableColsCons
    ∷ ( IsSymbol sym
      , R.Lacks sym r'
      , R.Cons sym (Col s t) r' r
      , TableCols tail r'
      )
    ⇒ TableCols (RL.Cons sym t tail) r
  where
  tableCols table _ = 
    let
      _sym = (SProxy ∷ SProxy sym)
      res' = tableCols table (RLProxy ∷ RLProxy tail)
      col = Col $ EColumn $ Column { table, name: reflectSymbol _sym }
    in
    Record.insert _sym col res'

--- Query

type GenState = 
  { sources ∷ Array AliasedTable
  , restricts ∷ Array (Expr Boolean)
  , nextId ∷ Int
  }

initState ∷ GenState
initState = 
  { sources: []
  , restricts: []
  , nextId: 0
  }

newtype Query s a = Query (State GenState a)
derive newtype instance functorQuery ∷ Functor (Query s)
derive newtype instance applyQuery ∷ Apply (Query s)
derive newtype instance applicativeQuery ∷ Applicative (Query s)
derive newtype instance bindQuery ∷ Bind (Query s)
derive newtype instance monadQuery ∷ Monad (Query s)

freshId ∷ ∀ s. Query s Int
freshId = Query do
  st ← get
  put $ st { nextId = st.nextId + 1 }
  pure st.nextId

---

newtype Col s a = Col (Expr a)
derive instance newtypeCol ∷ Newtype (Col s a) _

class Lit a where
  lit ∷ ∀ s. a → Col s a

instance litBoolean ∷ Lit Boolean where lit x = Col $ ELit $ LBoolean x identity
instance litString ∷ Lit String where lit x = Col $ ELit $ LString x identity
instance litInt ∷ Lit Int where lit x = Col $ ELit $ LInt x identity

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

instance showColumn ∷ Show (Column a) where
  show (Column { table, name }) = table.alias <> "." <> name

instance showCol ∷ Show (Col s a) where
  show = unwrap >>> show

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

