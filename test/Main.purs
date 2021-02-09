module Test.Main where

import Prelude

import Control.Monad.Cont (ContT(..), runContT)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.PG as PG
import Test.SQLite3 as SQLIte3
import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Test.UnitTests as Unit

main ∷ Effect Unit
main = do
  -- integration tests
  launchAff_ $ flip runContT pure do
    pg ← ContT PG.main
    sqlite3 ← ContT SQLIte3.main

    -- run test suites
    liftEffect $ runTest do
      Unit.testSuite
      suite "Selda" do
        pg
        sqlite3
