module Test.Main where

import Prelude

import Data.Eq (class EqRecord)
import Data.Maybe (Maybe(..))
import Data.Show (class ShowRecordFields)
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (class FromSQLRow, Connection, PoolConfiguration, defaultPoolConfiguration)
import Database.PostgreSQL as PG
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Prim.RowList as RL
import Selda (FullQuery, Table(..), aggregate, count, crossJoin, deleteFrom, groupBy, insert_, leftJoin, leftJoin_, lit, max_, query, restrict, selectFrom, selectFrom_, update, withPG, (.==), (.>))
import Selda.Col (class GetCols)
import Selda.PG.Utils (class ColsToPGHandler)
import Test.Unit (TestSuite, suite)
import Test.Unit.Main (runTest)
import Test.Utils (assertSeqEq, test)

people ∷ Table ( name ∷ String , age ∷ Maybe Int , id ∷ Int )
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
          age INTEGER
        );

        DROP TABLE IF EXISTS bank_accounts;
        CREATE TABLE bank_accounts (
          id INTEGER PRIMARY KEY,
          personId INTEGER NOT NULL,
          balance INTEGER NOT NULL
        );
      """) PG.Row0
      withPG dbconfig $ insert_ people 
        [ { id: 1, name: "name1", age: Just 11 }
        , { id: 2, name: "name2", age: Just 22 }
        , { id: 3, name: "name3", age: Just 33 }
        ]
      
      -- simple test delete
      withPG dbconfig do
        insert_ people [{ id: 4, name: "delete", age: Just 999 }]
        deleteFrom people \r → r.id .== lit 4

      -- simple test update
      withPG dbconfig do
        insert_ people [{ id: 5, name: "update", age: Just 999 }]
        update people
          (\r → r.name .== lit "update")
          (\r → r { age = lit $ Just 1000 })
        deleteFrom people \r → r.age .> lit (Just 999)

      -- PG.execute conn (PG.Query """
      --   INSERT INTO people (id, name, age)
      --   VALUES ($1, $2, $3), ($4, $5, $6), ($7, $8, $9)
      -- """)
      --   (( (1 /\ "name1" /\ 11)
      --   /\ (2 /\ "name2" /\ 22)
      --   /\ (3 /\ "name3" /\ 33)
      --   ))

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
          -- SELECT people_0.name AS name, people_0.id AS id, people_0.age AS age
          -- FROM people people_0
          test' conn "simple select people"
            [ { id: 1, name: "name1", age: Just 11 }
            , { id: 2, name: "name2", age: Just 22 }
            , { id: 3, name: "name3", age: Just 33 }
            ]
            $ selectFrom people \r → do
                pure r

          -- SELECT people_0.age AS y, people_0.id AS x
          -- FROM people people_0
          test' conn "select people, return different record"
            [ { x: 1, y: Just 11 }
            , { x: 2, y: Just 22 }
            , { x: 3, y: Just 33 }
            ]
            $ selectFrom people \{ id, age } → do
                pure { x: id, y: age }

          -- SELECT people_0.name AS name, people_0.id AS id, people_0.age AS age
          -- FROM people people_0 WHERE ((people_0.age > 20))
          test' conn "simple select people restrict"
            [ { id: 2, name: "name2", age: Just 22 }
            , { id: 3, name: "name3", age: Just 33 }
            ]
            $ selectFrom people \r@{ age } → do
                restrict $ age .> lit (Just 20)
                pure r

          -- SELECT people_0.id AS id1, people_1.age AS age2, people_0.age AS age1
          -- FROM people people_0, people people_1
          -- WHERE ((people_0.age > people_1.age))
          test' conn "cross product with restrict"
            [ { id1: 2, age1: Just 22, age2: Just 11 }
            , { id1: 3, age1: Just 33, age2: Just 11 }
            , { id1: 3, age1: Just 33, age2: Just 22 }
            ]
            $ selectFrom people \r1 → do
                r2 ← crossJoin people
                restrict $ r1.age .> r2.age
                pure { id1: r1.id, age1: r1.age, age2: r2.age }

          -- SELECT people_0.id AS id, bank_accounts_1.balance AS balance
          -- FROM people people_0, bank_accounts bank_accounts_1
          -- WHERE ((people_0.id = bank_accounts_1.personId))
          test' conn "cross product as natural join"
            [ { id: 1, balance: 100 }
            , { id: 1, balance: 150 }
            -- , { id: 2, balance: Nothing }
            , { id: 3, balance: 300 }
            ]
            $ selectFrom people \{ id, name, age } → do
                { balance, personId } ← crossJoin bankAccounts
                restrict $ id .== personId
                pure { id, balance }

          -- SELECT people_0.id AS id, bank_accounts_1.balance AS balance
          -- FROM people people_0
          -- LEFT JOIN bank_accounts bank_accounts_1
          --   ON ((people_0.id = bank_accounts_1.personId))
          test' conn "left join"
            [ { id: 1, balance: Just 100 }
            , { id: 1, balance: Just 150 }
            , { id: 2, balance: Nothing }
            , { id: 3, balance: Just 300 }
            ]
            $ selectFrom people \{ id, name, age } → do
                { balance } ← leftJoin bankAccounts \b → id .== b.personId
                pure { id, balance }

          -- SELECT people_0.id AS id, sub_q1.balance AS balance
          -- FROM people people_0
          -- LEFT JOIN
          --   ( SELECT 
          --       bank_accounts_0.personId AS personId,
          --       bank_accounts_0.id AS id,
          --       bank_accounts_0.balance AS balance
          --     FROM bank_accounts bank_accounts_0
          --   ) sub_q1
          --   ON ((people_0.id = sub_q1.personId))
          test' conn "left join but with subquery"
            [ { id: 1, balance: Just 100 }
            , { id: 1, balance: Just 150 }
            , { id: 2, balance: Nothing }
            , { id: 3, balance: Just 300 }
            ]
            $ selectFrom people \{ id, name, age } → do
                { balance } ← leftJoin_ (\b → id .== b.personId) do
                  selectFrom bankAccounts \b → do
                    -- restrict $ id .== b.personId -- type error
                    pure b
                pure { id, balance }

          -- SELECT max(people_0.id) AS maxId
          -- FROM people people_0
          test' conn "aggr: max people id"
            [ { maxId: 3 } ]
            $ selectFrom people \{ id, name, age } → aggregate do
                pure { maxId: max_ id }

          -- SELECT
          --   bank_accounts_0.personId AS pid,
          --   max(bank_accounts_0.balance) AS m,
          --   count(bank_accounts_0.personId) AS c
          -- FROM bank_accounts bank_accounts_0
          -- GROUP BY bank_accounts_0.personId
          test' conn "aggr: max people id"
            [ { pid: 1, m: 150, c: "2" }
            , { pid: 3, m: 300, c: "1" }
            ]
            $ selectFrom bankAccounts \{ personId, balance } → aggregate do
                pid ← groupBy personId
                pure { pid, m: max_ balance, c: count personId }

          -- SELECT
          --   sub_q0.pid AS pid,
          --   sub_q0.m AS m,
          --   sub_q0.c AS c
          -- FROM 
          --   ( SELECT
          --       bank_accounts_0.personId AS pid,
          --       max(bank_accounts_0.balance) AS m,
          --       count(bank_accounts_0.personId) AS c
          --     FROM bank_accounts bank_accounts_0
          --     GROUP BY bank_accounts_0.personId
          --   ) sub_q0
          -- WHERE ((sub_q0.c > '1'))
          test' conn "aggr: max people id having count > 1"
            [ { pid: 1, m: 150, c: "2" }
            ]
            $ selectFrom_ do
                selectFrom bankAccounts \{ personId, balance } → aggregate do
                    pid ← groupBy personId
                    pure { pid, m: max_ balance, c: count personId }
                $ \r@{ c } → do
                    restrict $ c .> lit "1"
                    pure r

test'
  ∷ ∀ s o i tup ol
  . ColsToPGHandler s i tup o
  ⇒ GetCols i ⇒ FromSQLRow tup
  ⇒ RL.RowToList o ol ⇒ ShowRecordFields ol o ⇒ EqRecord ol o
  ⇒ Connection → String → Array { | o } → FullQuery s { | i } → TestSuite
test' conn msg expected q = do
  test conn msg $ (withPG dbconfig $ query q) >>= assertSeqEq expected 

dbconfig ∷ PoolConfiguration
dbconfig = (defaultPoolConfiguration "purspg")
  { user = Just "init"
  , password = Just $ "qwerty"
  , idleTimeoutMillis = Just $ 1000
  }
