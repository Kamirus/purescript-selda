module SqlType where

import Prelude

import Data.Identity (Identity(..))

class Lit repr where
  litString ∷ String → repr String
  litInt ∷ Int → repr Int
  toSqlType ∷ ∀ a. repr a → SqlTypeRep

newtype LitI a = LitI { val ∷ a , sqlType ∷ SqlTypeRep }

instance lit ∷ Lit LitI where
  litString s = LitI { val: s, sqlType: TString }
  litInt s = LitI { val: s, sqlType: TInt }
  toSqlType (LitI r) = r.sqlType

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
