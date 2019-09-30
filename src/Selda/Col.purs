module Selda.Col where

import Prelude

import Data.Array ((:))
import Data.Exists (Exists, mkExists)
import Data.Functor.Variant (FProxy(..), VariantF, inj, on)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.RowList (kind RowList)
import Selda.Expr (Expr(..), Literal(..), None(..), Some(..), showExpr)
import Selda.Table (Alias, Column)
import Type.Data.Row (RProxy(..))
import Type.Proxy (Proxy)

newtype Col s v a = Col (VariantF v a)
derive instance newtypeCol ∷ Newtype (Col s v a) _

_expr = SProxy ∷ SProxy "expr"
type ExprF v = ( expr ∷ FProxy Expr | v )

onExpr
  ∷ ∀ s v a
  . (VariantF v a → String)
  → Col s ( expr ∷ FProxy Expr | v ) a
  → String
onExpr onRest = unwrap >>> on _expr showExpr onRest

type ExistsExpr v = Exists (VariantF v)

class Lit a where
  lit ∷ ∀ s. a → Col s ( ExprF () ) a
  literal ∷ a → Literal a

instance litBoolean ∷ Lit Boolean where
  literal x = LBoolean x identity
  lit x = Col $ inj _expr $ ELit $ literal x

instance litString ∷ Lit String where
  literal x = LString x identity
  lit x = Col $ inj _expr $ ELit $ literal x

instance litInt ∷ Lit Int where
  literal x = LInt x identity
  lit x = Col $ inj _expr $ ELit $ literal x

instance litMaybe ∷ Lit a ⇒ Lit (Maybe a) where
  literal = case _ of
    Nothing → LNull $ mkExists $ None identity
    Just l → LJust $ mkExists $ Some (literal l) identity
  lit x = Col $ inj _expr $ ELit $ literal x

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
instance toColsMapping
    ∷ Mapping (ToCols_ s) (Column a) (Col s ( expr ∷ FProxy Expr ) a)
  where
  mapping _ col = Col $ inj _expr $ EColumn col

-- | For record { n1 ∷ Col s v String, n2 ∷ Col s v String, id ∷ Col s v Int }
-- | → [(id, Expr Int), (n1, Expr String), (n2, Expr String)]
-- | → [(id, ExistsExpr v), (n1, ExistsExpr v), (n2, ExistsExpr v)]
class GetCols r v where
  getCols ∷ { | r } → RProxy v → Array (Tuple Alias (ExistsExpr v))
instance getcols 
    ∷ HFoldlWithIndex ExtractCols 
      (Array (Tuple String (ExistsExpr v)))
      { | r }
      (Array (Tuple String (ExistsExpr v))) 
    ⇒ GetCols r v
  where
  getCols r _ = hfoldlWithIndex ExtractCols ([] ∷ Array (Tuple String (ExistsExpr v))) r

data ExtractCols = ExtractCols
instance extractcols 
    ∷ IsSymbol sym 
    ⇒ FoldingWithIndex ExtractCols (SProxy sym) 
      (Array (Tuple String (Exists (VariantF v))))
      (Col s v a) 
      (Array (Tuple String (Exists (VariantF v))))
  where
  foldingWithIndex ExtractCols sym acc (Col e) = 
    Tuple (reflectSymbol (SProxy ∷ SProxy sym)) (mkExists e) : acc
