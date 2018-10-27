module BottomUp where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect (Effect)
import Effect.Console (log, logShow)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, foldingWithIndex, hfoldlWithIndex)

type Table = { name ∷ String , alias ∷ String }

t1 ∷ Table
t1 = { name: "persons", alias: "p1" }

newtype Col a = Col
  { table ∷ Table
  , name ∷ String
  }

nameCol ∷ Col String
nameCol = Col { table: t1, name: "name" }

idCol ∷ Col Int
idCol = Col { table: t1, name: "id" }

s1 = 
  { sources: [ t1 ]
  , restricts: []
  , res: { id: idCol, name: nameCol }
  }

data ShowNames = ShowNames

instance showNames
    ∷ IsSymbol sym
    ⇒ FoldingWithIndex ShowNames (SProxy sym) (Array String) a (Array String)
  where
  foldingWithIndex ShowNames prop acc a = 
    acc <> [reflectSymbol prop]

recordNames
  ∷ ∀ a
  . HFoldlWithIndex ShowNames (Array String) a (Array String)
  ⇒ a
  → (Array String)
recordNames r = hfoldlWithIndex ShowNames ([] ∷ Array String) r 

main ∷ Effect Unit
main = 
  let (names ∷ Array String) = recordNames { a: "xd", b: 1 } in
  logShow names


-- recordNames r = hfoldlWithIndex f x r

-- proc s = "select " <> cols <> " from " <> tables <> ";"
--   where
--   cols = 

{-

-}


