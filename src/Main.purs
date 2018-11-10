module Main where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Expr ((.==), (.>))
import PG.PG (runQuery)
import Query (restrict, select)
import Types (Table(..))

people ∷ Table ( name ∷ String , age ∷ Int , id ∷ Int )
people = Table { name: "people" }

main ∷ Effect Unit
main = launchAff_ $ do
  let
    q = do
      p1 ← select people
      p2 ← select people
      restrict $ p1.id .== p2.id
      restrict $ p1.id .> 1
      pure $ p1
  rows ← runQuery q
  liftEffect $ log "id\tn1\tn2"
  -- liftEffect $ for_ rows \{ id, n1, n2 } → log (show id <> "\t" <> n1 <> "\t" <> n2 )
  liftEffect $ for_ rows \r → log (show r)
