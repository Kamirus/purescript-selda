module Test.Main where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Database.PostgreSQL (PoolConfiguration)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Selda (Col, Query, Table(..), lit, restrict, select, withPG, (.==), (.>))

people ∷ Table ( name ∷ String , age ∷ Int , id ∷ Int )
people = Table { name: "people" }

q ∷ ∀ s. Query s { n1 ∷ Col s String , id ∷ Col s Int , n2 ∷ Col s String }
q = do
  p1 ← select people
  p2 ← select people
  restrict $ p1.id .== p2.id
  restrict $ p1.id .> lit 1
  pure $ { id: p1.id, n1: p1.name, n2: p2.name }

-- q1 ∷ ∀ s. Query s { id ∷ Col s Int , name ∷ Col s String , age ∷ Col s Int }
q1 = do
  {id, name, age} ← select people
  pure $ {a: name, b: name, c: id}

q2 = do
  p1 ← select people
  p2 ← select people
  pure $ { p1, p2 }

q3 ∷ ∀ s. Query s { l ∷ Col s Int , l2 ∷ Col s Int }
q3 = pure $ { l: lit 123, l2: lit 123 }

main ∷ Effect Unit
main = do 
  launchAff_ $ do
    rows ← withPG q dbconfig
    liftEffect $ log "id\tn1\tn2"
    liftEffect $ for_ rows \{ id, n1, n2 } → log (show id <> "\t" <> n1 <> "\t" <> n2 )
  m1 
  m3

m1 = launchAff_ $ do
  rows ← withPG q1 dbconfig
  liftEffect $ for_ rows \r → log (show r)

-- m2 = launchAff_ $ do
--   rows ← withPG q2 dbconfig
--   liftEffect $ for_ rows \r → log (show r)

m3 = launchAff_ $ do
  rows ← withPG q3 dbconfig
  liftEffect $ for_ rows \r → log (show r)

dbconfig ∷ PoolConfiguration
dbconfig =	
  { database: "selda"	
  , host: Just $ "127.0.0.1"	
  , idleTimeoutMillis: Just $ 1000	
  , max: Just $ 10	
  , password: Just $ "qwerty"	
  , port: Just $ 5432	
  , user: Just $ "init"	
  }
