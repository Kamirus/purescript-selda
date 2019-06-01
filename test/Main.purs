module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Eq (class EqRecord)
import Data.Maybe (Maybe(..))
import Data.Show (class ShowRecordFields)
import Database.PostgreSQL (class FromSQLRow, Connection, PoolConfiguration, defaultPoolConfiguration)
import Database.PostgreSQL as PG
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Global.Unsafe (unsafeStringify)
import Prim.RowList as RL
import Selda (FullQuery, Table(..), aggregate, count, crossJoin, deleteFrom, desc, groupBy, insert_, leftJoin, leftJoin_, limit, lit, max_, orderBy, query, restrict, selectFrom, selectFrom_, update, (.==), (.>))
import Selda.Col (class GetCols)
import Selda.PG.Utils (class ColsToPGHandler)
import Selda.Query (notNull)
import Test.Unit (TestSuite, failure, suite)
import Test.Unit.Main (runTest)
import Test.Utils (assertSeqEq, assertUnorderedSeqEq, runSeldaAff, test)

people ∷ Table ( name ∷ String , age ∷ Maybe Int , id ∷ Int )
people = Table { name: "people" }

bankAccounts ∷ Table ( personId ∷ Int, id ∷ Int, balance ∷ Int )
bankAccounts = Table { name: "bank_accounts" }

descriptions ∷ Table ( id ∷ Int, text ∷ Maybe String )
descriptions = Table { name: "descriptions" }

emptyTable ∷ Table ( id ∷ Int )
emptyTable = Table { name: "emptyTable" }

main ∷ Effect Unit
main = do
  pool ← PG.newPool dbconfig
  void $ launchAff do
    PG.withConnection pool case _ of
      Left pgError → failure ("PostgreSQL connection error: " <> unsafeStringify pgError)
      Right conn → do
        void $ PG.execute conn (PG.Query """
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

          DROP TABLE IF EXISTS descriptions;
          CREATE TABLE descriptions (
            id INTEGER PRIMARY KEY,
            text TEXT
          );

          DROP TABLE IF EXISTS emptyTable;
          CREATE TABLE emptyTable (
            id INTEGER PRIMARY KEY
          );
        """) PG.Row0

        runSeldaAff conn do
          insert_ people
            [ { id: 1, name: "name1", age: Just 11 }
            , { id: 2, name: "name2", age: Just 22 }
            , { id: 3, name: "name3", age: Just 33 }
            ]
          insert_ bankAccounts
            [ { id: 1, personId: 1, balance: 100 }
            , { id: 2, personId: 1, balance: 150 }
            , { id: 3, personId: 3, balance: 300 }
            ]
          insert_ descriptions
            [ { id: 1, text: Just "text1" }
            , { id: 3, text: Nothing }
            ]

        -- simple test delete
        runSeldaAff conn do
          insert_ people [{ id: 4, name: "delete", age: Just 999 }]
          deleteFrom people \r → r.id .== lit 4

        -- simple test update
        runSeldaAff conn do
          insert_ people [{ id: 5, name: "update", age: Just 999 }]
          update people
            (\r → r.name .== lit "update")
            (\r → r { age = lit $ Just 1000 })
          deleteFrom people \r → r.age .> lit (Just 999)

        liftEffect $ runTest $ do
          suite "Selda" $ do
            test' conn "simple select people"
              [ { id: 1, name: "name1", age: Just 11 }
              , { id: 2, name: "name2", age: Just 22 }
              , { id: 3, name: "name3", age: Just 33 }
              ]
              $ selectFrom people \r → do
                  pure r

            test' conn "select people, return different record"
              [ { x: 1, y: Just 11 }
              , { x: 2, y: Just 22 }
              , { x: 3, y: Just 33 }
              ]
              $ selectFrom people \{ id, age } → do
                  pure { x: id, y: age }

            test' conn "simple select people restrict"
              [ { id: 2, name: "name2", age: Just 22 }
              , { id: 3, name: "name3", age: Just 33 }
              ]
              $ selectFrom people \r@{ age } → do
                  restrict $ age .> lit (Just 20)
                  pure r

            test' conn "cross product with restrict"
              [ { id1: 2, age1: Just 22, age2: Just 11 }
              , { id1: 3, age1: Just 33, age2: Just 11 }
              , { id1: 3, age1: Just 33, age2: Just 22 }
              ]
              $ selectFrom people \r1 → do
                  r2 ← crossJoin people
                  restrict $ r1.age .> r2.age
                  pure { id1: r1.id, age1: r1.age, age2: r2.age }

            test' conn "leftJoin: just Maybe Int insead of Maybe Maybe Int "
              [ { id: 1
                -- , age1: Just 11, age2: Just $ Just 11
                , age1: Just 11, age2: Just 11
                , name1: "name1", name2: Just "name1" }
              ]
              $ selectFrom people \r1 → do
                  r2 ← leftJoin people \{ id } → id .== r1.id
                  restrict $ r1.id .== lit 1
                  -- error
                  -- orderBy asc $ max_ r1.id
                  pure
                    { id: r1.id
                    , age1: r1.age, age2: r2.age
                    , name1: r1.name, name2: r2.name }

            test' conn "leftJoin maybe column: Just Nothing vs Nothing"
              [ { id: 1, text: Just "text1" }
              , { id: 2, text: Nothing }
              , { id: 3, text: Nothing }
              -- , { id: 3, text: Just Nothing }
              ]
              $ selectFrom people \r → do
                  { text } ← leftJoin descriptions \{ id } → r.id .== id
                  pure { id: r.id, text }

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

            test' conn "left join"
              [ { id: 1, balance: Just 100 }
              , { id: 1, balance: Just 150 }
              , { id: 2, balance: Nothing }
              , { id: 3, balance: Just 300 }
              ]
              $ selectFrom people \{ id, name, age } → do
                  { balance } ← leftJoin bankAccounts \b → id .== b.personId
                  pure { id, balance }

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

            test' conn "subquery with aggregate max"
              [ { balance: Just 150, id: 1 }
              , { balance: Nothing, id: 2 }
              , { balance: Just 300, id: 3 }
              ] $ selectFrom people \{ id, name, age } -> do
                    { balance } <- leftJoin_ (\b -> id .== b.personId) $
                      aggregate $ selectFrom bankAccounts \b -> do
                        personId <- groupBy b.personId
                        -- restrict $ id .> lit 1
                        pure { personId, balance: max_ b.balance }
                    pure { id, balance }

            test' conn "aggr: max people id"
              [ { maxId: Just 3 } ]
              $ aggregate $ selectFrom people \{ id, name, age } → do
                  pure { maxId: max_ id }

            test' conn "aggr: max people id"
              [ { pid: 1, m: Just 150, c: "2" }
              , { pid: 3, m: Just 300, c: "1" }
              ]
              $ aggregate $ selectFrom bankAccounts \{ personId, balance } → do
                  pid ← groupBy personId
                  pure { pid, m: max_ balance, c: count personId }

            testWith assertSeqEq conn "aggr: order by max people id desc"
              [ { pid: 3, m: 300, c: "1" }
              , { pid: 1, m: 150, c: "2" }
              ]
              $ selectFrom_ do
                  aggregate $ selectFrom bankAccounts \{ personId, balance } → do
                      pid ← groupBy personId
                      pure { pid, m: max_ balance, c: count personId }
                  $ \r@{ pid, c } → do
                      m ← notNull r.m
                      orderBy desc m
                      pure { pid, m, c }

            test' conn "aggr: max people id having count > 1"
              [ { pid: 1, m: 150, c: "2" }
              ]
              $ selectFrom_ do
                  aggregate $ selectFrom bankAccounts \{ personId, balance } → do
                      pid ← groupBy personId
                      pure { pid, m: max_ balance, c: count personId }
                  $ \r@{ pid, c } → do
                      m ← notNull r.m
                      restrict $ c .> lit "1"
                      pure { pid, m, c }

            test' conn "limit negative returns 0"
              [ ]
              $ selectFrom people \r → do
                  limit $ -7
                  pure r

            test' conn "limit + order by: return first"
              [ { pid: 3, maxBalance: Just 300 } ]
              $ aggregate $ selectFrom bankAccounts \{ personId, balance } → do
                  pid ← groupBy personId
                  limit 1
                  orderBy desc personId
                  pure { pid, maxBalance: max_ balance }

            test' conn "max(id) on empty table returns 1 result: null"
              [ { maxId: Nothing } ]
              $ aggregate $ selectFrom emptyTable \r → pure { maxId: max_ r.id }

            test' conn "max(id) on empty table returns 0 results with notNull"
              [ ]
              $ selectFrom_ do 
                  aggregate $ selectFrom emptyTable \r →
                      pure { maxId: max_ r.id }
                  $ \r → do
                      id ← notNull r.maxId
                      pure { id }

            test' conn "return only not null values"
              [ { id: 1, text: "text1" } ]
              $ selectFrom descriptions \ { id, text: maybeText } → do
                  text ← notNull maybeText
                  pure { id, text }

test'
  ∷ ∀ s o i tup ol
  . ColsToPGHandler s i tup o
  ⇒ GetCols i ⇒ FromSQLRow tup
  ⇒ RL.RowToList o ol ⇒ ShowRecordFields ol o ⇒ EqRecord ol o
  ⇒ Connection → String → Array { | o } → FullQuery s { | i } → TestSuite
test' = testWith assertUnorderedSeqEq

testWith
  ∷ ∀ s o i tup ol
  . ColsToPGHandler s i tup o
  ⇒ GetCols i ⇒ FromSQLRow tup
  ⇒ RL.RowToList o ol ⇒ ShowRecordFields ol o ⇒ EqRecord ol o
  ⇒ (Array { | o } → Array { | o } → Aff Unit) 
  → Connection
  → String
  → Array { | o }
  → FullQuery s { | i }
  → TestSuite
testWith assertFunc conn msg expected q = do
  test conn msg $ (runSeldaAff conn $ query q) >>= assertFunc expected

dbconfig ∷ PoolConfiguration
dbconfig = (defaultPoolConfiguration "purspg")
  { user = Just "init"
  , password = Just $ "qwerty"
  , idleTimeoutMillis = Just $ 1000
  }
