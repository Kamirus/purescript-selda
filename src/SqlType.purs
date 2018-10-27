module SqlType where

import Prelude

import Data.Identity (Identity(..))
import Data.Newtype (class Newtype)

data LitType a
  = LString String
  | LInt Int

class LitC repr where
  litString ∷ String → repr String
  litInt ∷ Int → repr Int
  toRepr ∷ ∀ a. repr a → LitType a

-- newtype Lit a = Lit { val ∷ a , type ∷ LitType }
-- derive instance litNewtype ∷ Newtype (Lit a) _

-- instance lit ∷ LitC Lit where
--   litString s = Lit { val: s, type: LString }
--   litInt s = Lit { val: s, type: LInt }
--   toRepr r = r

instance lit ∷ LitC LitType where
  litString s = LString s
  litInt s = LInt s
  toRepr r = r


eval ∷ ∀ a. LitType a → String
eval = case _ of
  LString s -> s
  LInt i → show i

---

data SqlTypeRep
  = TString
  | TInt

---

-- class Lit lit a where
--   toLit ∷ a → lit a
--   litType ∷ lit a → SqlTypeRep

-- newtype LitI a = LitI a

-- instance litInt ∷ Lit LitI Int where
--   toLit i = LitI i
--   litType _ = TInt
-- else instance litString ∷ Lit LitI String where
--   toLit i = LitI i
--   litType _ = TString
