module Selda.Col
  ( Col(..), showCol
  , class Lit
  , class GetCols, getCols
  , ExtractCols(..)
  , lit
  , class ToCols, toCols
  -- , class ExtractCols, extractCols
  ) where

import Prelude

import Data.Array ((:))
import Data.Exists (Exists, mkExists)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, foldingWithIndex, hfoldlWithIndex)
import Prim.Row as R
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Selda.Expr (Expr(..), Literal(..), showExpr)
import Selda.Table (Alias, Column)
import Type.Proxy (Proxy)
import Type.Row (RLProxy(..))

newtype Col s a = Col (Expr a)
derive instance newtypeCol ∷ Newtype (Col s a) _

showCol ∷ ∀ s a. Col s a → String
showCol = unwrap >>> showExpr

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
  toCols s i _ = Record.insert _sym (Col $ EColumn col) res'
    where
    _sym = (SProxy ∷ SProxy sym)
    col = Record.get _sym i
    res' = toCols s i (RLProxy ∷ RLProxy tail)

{- 
For record { n1 ∷ Col s String, n2 ∷ Col s String, id ∷ Col s Int }
→ [(id, Expr Int), (n1, Expr String), (n2, Expr String)]
→ [(id, Exists Expr), (n1, Exists Expr), (n2, Exists Expr)]
-}
class GetCols r where
  getCols ∷ { | r } → Array (Tuple Alias (Exists Expr))
instance getcols 
    ∷ HFoldlWithIndex ExtractCols 
      (Array (Tuple String (Exists Expr)))
      { | r }
      (Array (Tuple String (Exists Expr))) 
    ⇒ GetCols r
  where
  getCols r = hfoldlWithIndex ExtractCols ([] ∷ Array (Tuple String (Exists Expr))) r
-- getCols i = extractCols i (RLProxy ∷ RLProxy il)

data ExtractCols = ExtractCols
instance extractcols 
    ∷ IsSymbol sym 
    ⇒ FoldingWithIndex ExtractCols (SProxy sym) 
      (Array (Tuple String (Exists Expr)))
      (Col s a) 
      (Array (Tuple String (Exists Expr)))
  where
  foldingWithIndex ExtractCols sym acc (Col e) = 
    Tuple (reflectSymbol (SProxy ∷ SProxy sym)) (mkExists e) : acc

-- class ExtractCols (i ∷ # Type) (il ∷ RowList) where
--   extractCols
--     ∷ Record i
--     → RLProxy il
--     → Array (Tuple Alias (Exists Expr))

-- instance extractColsHead
--     ∷ ( IsSymbol sym
--       , R.Cons sym (Col s t) i' i
--       )
--     ⇒ ExtractCols i (RL.Cons sym (Col s t) RL.Nil)
--   where
--   extractCols i _ = [ Tuple (reflectSymbol _sym) (mkExists e) ]
--     where
--     _sym = (SProxy ∷ SProxy sym)
--     Col e = Record.get _sym i

-- else instance extractColsCons
--     ∷ ( IsSymbol sym
--       , R.Cons sym (Col s t) i' i
--       , ExtractCols i tail
--       )
--     ⇒ ExtractCols i (RL.Cons sym (Col s t) tail)
--   where
--   extractCols i _ = cols <> [Tuple (reflectSymbol _sym) (mkExists e)]
--     where
--     _sym = (SProxy ∷ SProxy sym)
--     cols = extractCols i (RLProxy ∷ RLProxy tail)
--     Col e = Record.get _sym i
