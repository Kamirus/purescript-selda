module Selda.Col where

import Prelude

import Data.Array ((:))
import Data.Exists (Exists, mkExists)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.RowList (kind RowList)
import Selda.Expr (Expr(..), Literal(..), None(..), Some(..), showExpr)
import Selda.Table (Alias, Column)
import Type.Proxy (Proxy)

newtype Col s a = Col (Expr a)
derive instance newtypeCol ∷ Newtype (Col s a) _

showCol ∷ ∀ s a. Col s a → String
showCol = unwrap >>> showExpr

class Lit a where
  lit ∷ ∀ s. a → Col s a
  literal ∷ a → Literal a

instance litBoolean ∷ Lit Boolean where
  literal x = LBoolean x identity
  lit x = Col $ ELit $ literal x

instance litString ∷ Lit String where
  literal x = LString x identity
  lit x = Col $ ELit $ literal x

instance litInt ∷ Lit Int where
  literal x = LInt x identity
  lit x = Col $ ELit $ literal x

instance litMaybe ∷ Lit a ⇒ Lit (Maybe a) where
  literal = case _ of
    Nothing → LNull $ mkExists $ None identity
    Just l → LJust $ mkExists $ Some (literal l) identity
  lit x = Col $ ELit $ literal x

-- | ```purescript
-- | { name ∷ Column String, id ∷ Column Int }
-- | → 
-- | { name ∷ Col s String, id ∷ Col s Int }
-- | ```
class ToCols s i o | s i → o where
  toCols ∷ Proxy s → { | i } → { | o }

instance toColsI ∷ HMap (ToCols_ s) { | i } { | o } ⇒ ToCols s i o where
  toCols _ = hmap (ToCols_ ∷ ToCols_ s)

data ToCols_ s = ToCols_
instance toColsMapping ∷ Mapping (ToCols_ s) (Column a) (Col s a) where
  mapping _ col = Col $ EColumn col

-- | For record { n1 ∷ Col s String, n2 ∷ Col s String, id ∷ Col s Int }
-- | → [(id, Expr Int), (n1, Expr String), (n2, Expr String)]
-- | → [(id, Exists Expr), (n1, Exists Expr), (n2, Exists Expr)]
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
