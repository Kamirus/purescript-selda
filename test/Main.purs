module Test.Main where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (PoolConfiguration, Row0(..), defaultPoolConfiguration)
import Database.PostgreSQL as PG
import Effect (Effect)
import Effect.Aff (launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Selda (Table(..), leftJoin, leftJoin', lit, restrict, select, withPG, (.==), (.>))
import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Test.Utils (assertSeqEq, test)

people ∷ Table ( name ∷ String , age ∷ Int , id ∷ Int )
people = Table { name: "people" }

bankAccounts ∷ Table ( personId ∷ Int, id ∷ Int, balance ∷ Int )
bankAccounts = Table { name: "bank_accounts" }

main ∷ Effect Unit
main = do
  void $ launchAff do
    pool <- PG.newPool dbconfig
    PG.withConnection pool \conn -> do
      PG.execute conn (PG.Query """
        DROP TABLE IF EXISTS people;
        CREATE TABLE people (
          id INTEGER PRIMARY KEY,
          name TEXT NOT NULL,
          age INTEGER NULL
        );

        DROP TABLE IF EXISTS bank_accounts;
        CREATE TABLE bank_accounts (
          id INTEGER PRIMARY KEY,
          personId INTEGER NOT NULL,
          balance INTEGER NOT NULL
        );
      """) PG.Row0
      PG.execute conn (PG.Query """
        INSERT INTO people (id, name, age)
        VALUES ($1, $2, $3), ($4, $5, $6), ($7, $8, $9)
      """)
        (( (1 /\ "name1" /\ 11)
        /\ (2 /\ "name2" /\ 22)
        /\ (3 /\ "name3" /\ 33)
        ))

      PG.execute conn (PG.Query """
        INSERT INTO bank_accounts (id, personId, balance)
        VALUES ($1, $2, $3), ($4, $5, $6), ($7, $8, $9)
      """)
        (( (1 /\ 1 /\ 100)
        /\ (2 /\ 1 /\ 150)
        /\ (3 /\ 3 /\ 300)
        ))

      liftEffect $ runTest $ do
        suite "Selda" $ do
          test conn "simple select people" $ do
            let
              expected = 
                [ { id: 1, name: "name1", age: 11 }
                , { id: 2, name: "name2", age: 22 }
                , { id: 3, name: "name3", age: 33 }
                ]
            rows <- withPG dbconfig $ do
              r ← select people
              pure r
            assertSeqEq expected rows
          test conn "select people, return different record" $ do
            let
              expected = 
                [ { x: 1, y: 11 }
                , { x: 2, y: 22 }
                , { x: 3, y: 33 }
                ]
            rows <- withPG dbconfig $ do
              { id, age } ← select people
              pure { x: id, y: age }
            assertSeqEq expected rows
          test conn "simple select people restrict" $ do
            let
              expected = 
                [ { id: 2, name: "name2", age: 22 }
                , { id: 3, name: "name3", age: 33 }
                ]
            rows <- withPG dbconfig $ do
              r@{ age } ← select people
              restrict $ age .> lit 20
              pure r
            assertSeqEq expected rows
          test conn "cross product with restrict" $ do
            let
              expected = 
                [ { id1: 2, age1: 22, age2: 11 }
                , { id1: 3, age1: 33, age2: 11 }
                , { id1: 3, age1: 33, age2: 22 }
                ]
            rows <- withPG dbconfig $ do
              r1 ← select people
              r2 ← select people
              restrict $ r1.age .> r2.age
              pure { id1: r1.id, age1: r1.age, age2: r2.age }
            assertSeqEq expected rows
          test conn "cross product as natural join" $ do
            let
              expected = 
                [ { id: 1, balance: 100 }
                , { id: 1, balance: 150 }
                -- , { id: 2, balance: Nothing }
                , { id: 3, balance: 300 }
                ]
            rows <- withPG dbconfig $ do
              { id, name, age } ← select people
              { balance, personId } ← select bankAccounts
              restrict $ id .== personId
              pure { id, balance }
            assertSeqEq expected rows
          test conn "left join" $ do
            let
              expected = 
                [ { id: 1, balance: Just 100 }
                , { id: 1, balance: Just 150 }
                , { id: 2, balance: Nothing }
                , { id: 3, balance: Just 300 }
                ]
            rows <- withPG dbconfig $ do
              { id, name, age } ← select people
              { balance } ← leftJoin bankAccounts \b → id .== b.personId
              pure { id, balance }
            assertSeqEq expected rows
          test conn "alone left join transformed to select but with every value wrapped in Just" $ do
            let
              expected = 
                [ { id: Just 1, personId: Just 1, balance: Just 100 }
                , { id: Just 2, personId: Just 1, balance: Just 150 }
                , { id: Just 3, personId: Just 3, balance: Just 300 }
                ]
            rows <- withPG dbconfig $ do
              { id, personId, balance } ← leftJoin bankAccounts \_ → lit true
              pure { id, balance, personId }
            assertSeqEq expected rows
          test conn "left join but with subquery" $ do
            let
              expected = 
                [ { id: 1, balance: Just 100 }
                , { id: 1, balance: Just 150 }
                , { id: 2, balance: Nothing }
                , { id: 3, balance: Just 300 }
                ]
            rows <- withPG dbconfig $ do
              { id, name, age } ← select people
              { balance } ← leftJoin' (\b → id .== b.personId) do
                b ← select bankAccounts
                -- restrict $ id .== b.personId -- type error
                pure b
              pure { id, balance }
            assertSeqEq expected rows
          -- test conn "test" $ do
          --   (rows ∷ Array (_ Int String Int String Int)) ← PG.query conn (PG.Query """
          --     select p.id, count(p.name), p.age, count(b.id), max(b.personId) -- b.id, b.personId, b.balance
          --     from people p, bank_accounts b
          --     where p.id > 0 and b.id > 0
          --     -- group by p.id, p.name, p.age, b.personId
          --   """) Row0
          --   liftEffect $ log ""
          --   liftEffect $ for_ rows \(PG.Row5 pid name age bid bpid) → do
          --     log $ show pid <> " " <> show name <> " " <> show age <> " " <> show bid <> " " <> show bpid -- <> " " <> show balance
          --   pure unit

main' ∷ Effect Unit
main' = do 
  launchAff_ $ do
    rows ← withPG dbconfig $ do
      p1 ← select people
      p2 ← select people
      restrict $ p1.id .== p2.id
      restrict $ p1.id .> lit 1
      pure $ { id: p1.id, n1: p1.name, n2: p2.name }
    liftEffect $ log "id\tn1\tn2"
    liftEffect $ for_ rows \{ id, n1, n2 } → log (show id <> "\t" <> n1 <> "\t" <> n2 )

dbconfig ∷ PoolConfiguration
dbconfig = (defaultPoolConfiguration "purspg")
  { user = Just "init"
  , password = Just $ "qwerty"
  , idleTimeoutMillis = Just $ 1000
  }
