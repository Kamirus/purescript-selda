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
import Selda (Query, Table(..), aggregate, count, groupBy, leftJoin, leftJoin', lit, max_, restrict, select, withPG, (.==), (.>))
import Selda.Col (class ExtractCols)
import Selda.PG (class BuildPGHandler, class ColsToPGHandler)
import Test.Unit (TestSuite, suite)
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
          test' conn "simple select people"
            [ { id: 1, name: "name1", age: 11 }
            , { id: 2, name: "name2", age: 22 }
            , { id: 3, name: "name3", age: 33 }
            ]
            $ do
              r ← select people
              pure r

          test' conn "select people, return different record"
            [ { x: 1, y: 11 }
            , { x: 2, y: 22 }
            , { x: 3, y: 33 }
            ]
            $ do
              { id, age } ← select people
              pure { x: id, y: age }

          test' conn "simple select people restrict"
            [ { id: 2, name: "name2", age: 22 }
            , { id: 3, name: "name3", age: 33 }
            ]
            $ do
              r@{ age } ← select people
              restrict $ age .> lit 20
              pure r

          test' conn "cross product with restrict"
            [ { id1: 2, age1: 22, age2: 11 }
            , { id1: 3, age1: 33, age2: 11 }
            , { id1: 3, age1: 33, age2: 22 }
            ]
            $ do
              r1 ← select people
              r2 ← select people
              restrict $ r1.age .> r2.age
              pure { id1: r1.id, age1: r1.age, age2: r2.age }

          test' conn "cross product as natural join"
            [ { id: 1, balance: 100 }
            , { id: 1, balance: 150 }
            -- , { id: 2, balance: Nothing }
            , { id: 3, balance: 300 }
            ]
            $ do
              { id, name, age } ← select people
              { balance, personId } ← select bankAccounts
              restrict $ id .== personId
              pure { id, balance }

          test' conn "left join"
            [ { id: 1, balance: Just 100 }
            , { id: 1, balance: Just 150 }
            , { id: 2, balance: Nothing }
            , { id: 3, balance: Just 300 }
            ]
            $ do
              { id, name, age } ← select people
              { balance } ← leftJoin bankAccounts \b → id .== b.personId
              pure { id, balance }

          test' conn "alone left join transformed to select but with every value wrapped in Just"
            [ { id: Just 1, personId: Just 1, balance: Just 100 }
            , { id: Just 2, personId: Just 1, balance: Just 150 }
            , { id: Just 3, personId: Just 3, balance: Just 300 }
            ]
            $ do
              { id, personId, balance } ← leftJoin bankAccounts \_ → lit true
              pure { id, balance, personId }

          test' conn "left join but with subquery"
            [ { id: 1, balance: Just 100 }
            , { id: 1, balance: Just 150 }
            , { id: 2, balance: Nothing }
            , { id: 3, balance: Just 300 }
            ]
            $ do
              { id, name, age } ← select people
              { balance } ← leftJoin' (\b → id .== b.personId) do
                b ← select bankAccounts
                -- restrict $ id .== b.personId -- type error
                pure b
              pure { id, balance }

          test' conn "aggr: max people id"
            [ { maxId: 3 } ]
            $ aggregate do
              { id, name, age } ← select people
              pure { maxId: max_ id }

          test' conn "aggr: max people id"
            [ { pid: 1, m: 150, c: "2" }
            , { pid: 3, m: 300, c: "1" }
            ]
            $ aggregate do
              { personId, balance } ← select bankAccounts
              pid ← groupBy personId
              pure { pid, m: max_ balance, c: count personId }

          test' conn "aggr: max people id having count > 1"
            [ { pid: 1, m: 150, c: "2" }
            ]
            $ do
              r@{ c } ← aggregate do
                { personId, balance } ← select bankAccounts
                pid ← groupBy personId
                pure { pid, m: max_ balance, c: count personId }
              restrict $ c .> lit "1"
              pure r

test'
  ∷ ∀ s o i il tup ol
  . ColsToPGHandler s i tup o
  ⇒ RL.RowToList i il ⇒ ExtractCols i il ⇒ FromSQLRow tup
  ⇒ RL.RowToList o ol ⇒ ShowRecordFields ol o ⇒ EqRecord ol o
  ⇒ Connection → String → Array { | o } → Query s { | i } → TestSuite
test' conn msg expected q = do
  test conn msg $ withPG dbconfig q >>= assertSeqEq expected 

dbconfig ∷ PoolConfiguration
dbconfig = (defaultPoolConfiguration "purspg")
  { user = Just "init"
  , password = Just $ "qwerty"
  , idleTimeoutMillis = Just $ 1000
  }
