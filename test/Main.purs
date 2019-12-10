module Test.Main where

import Prelude

import Control.Monad.Cont (ContT(..), runContT)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Guide.SimpleE2E as Guide.SimpleE2E
import Test.PG as PG
import Test.SQLite3 as SQLIte3
import Test.Unit (suite)
import Test.Unit.Main (runTest)

main ∷ Effect Unit
main = do
  -- run literate guides
  Guide.SimpleE2E.main

  launchAff_ $ flip runContT pure do
    pg ← ContT PG.main
    sqlite3 ← ContT SQLIte3.main

    -- run test suites
    liftEffect $ runTest $ suite "Selda" do
      pg
      sqlite3
