module Selda.Lit where

import Prelude

import Data.Exists (mkExists)
import Data.Maybe (Maybe(..))
import Selda.Aggr (class Coerce, unsafeFromCol)
import Selda.Col (Col(..))
import Selda.Expr (Expr(..), Literal(..), None(..), Some(..))

-- | Lift a value `a` to a column expression using `Lit a` typeclass.
-- | Defined only for basic literals: Boolean, String, Int and Maybe.
-- | To handle more cases refer to the function `litPG`.
lit ∷ ∀ col s a. Lit a ⇒ Coerce col ⇒ a → col s a
lit = unsafeFromCol <<< Col <<< ELit <<< literal

class Lit a where
  literal ∷ a → Literal a

instance litBoolean ∷ Lit Boolean where
  literal x = LBoolean x identity

instance litString ∷ Lit String where
  literal x = LString x identity

instance litInt ∷ Lit Int where
  literal x = LInt x identity

instance litMaybe ∷ Lit a ⇒ Lit (Maybe a) where
  literal = case _ of
    Nothing → LNull $ mkExists $ None identity
    Just l → LJust $ mkExists $ Some (literal l) identity
