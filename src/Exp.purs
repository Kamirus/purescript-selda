module Exp where

import SqlType (class Lit)

class Exp exp sql a where
  col ∷ String → exp sql a
  lit ∷ ∀ lit. Lit lit a ⇒ lit a → exp sql a
