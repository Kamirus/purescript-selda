module Test.Main where

import Prelude

import Effect (Effect)
import Guide.SimpleE2E as Guide.SimpleE2E
import Test.PG as PG
import Test.SQLite3 as SQLIte3

main ∷ Effect Unit
main = do
  -- run literate guides
  Guide.SimpleE2E.main
  PG.main
  SQLIte3.main
