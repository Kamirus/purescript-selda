module Test.PG where

import Prelude

import Data.Date (Date, canonicalDate)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Database.PostgreSQL (PoolConfiguration, defaultPoolConfiguration)
import Database.PostgreSQL as PostgreSQL
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafePartial)
import Selda (Col, Table(..), lit, restrict, selectFrom, (.==), (.>))
import Selda.PG (extract, litPG)
import Selda.PG.Class (deleteFrom, insert, insert1, insert1_, insert_, update)
import Selda.Query.Class (class GenericQuery)
import Selda.Table.Constraint (Auto, Default)
import Test.Common (bankAccounts, descriptions, legacySuite, people)
import Test.Types (AccountType(..))
import Test.Unit (TestSuite, failure, suite)
import Test.Utils (class TestBackend, TestCtx, assertSeqEq, assertUnorderedSeqEq, runSeldaAff, testWith, testWithPG)

employees ∷ Table
  ( id ∷ Auto Int
  , name ∷ String
  , salary ∷ Default Int
  , date ∷ Default Date
  )
employees = Table { name: "employees" }

-- | Table with a problematic column name in Postgresql. Only for querying
pgKeywordTable ∷ Table ( end ∷ Int )
pgKeywordTable = Table { name: "pg_keyword_table" }

-- | Table with a problematic column name in Postgresql
-- | with manually escaped column name.
-- | Use for insert/update/delete. Not safe for querying though.
pgKeywordTable_quote ∷ Table ( "\"end\"" ∷ Int )
pgKeywordTable_quote = Table t
  where (Table t) = pgKeywordTable

date ∷ Int → Int → Int → Date
date y m d = unsafePartial $ fromJust $
  canonicalDate <$> toEnum y <*> toEnum m <*> toEnum d

testSuite
  ∷ ∀ b m ctx
  . TestBackend b m ctx
  ⇒ GenericQuery b m
      ( date ∷ Col Unit Date, id ∷ Col Unit Int, name ∷ Col Unit String, salary ∷ Col Unit Int )
      ( date ∷ Date, id ∷ Int, name ∷ String, salary ∷ Int )
  ⇒ GenericQuery b m
      ( y ∷ Col Unit Int, m ∷ Col Unit Int, d ∷ Col Unit Int )
      ( y ∷ Int, m ∷ Int, d ∷ Int )
  ⇒ GenericQuery b m
      ( end ∷ Col Unit Int )
      ( end ∷ Int )
  ⇒ TestCtx b m ctx
  → TestSuite
testSuite ctx = do
  let
    unordered = assertUnorderedSeqEq
    ordered = assertSeqEq

  testWith ctx unordered "employees inserted with default and without salary"
    [ { id: 1, name: "E1", salary: 123, date: date 2000 10 20 }
    , { id: 2, name: "E2", salary: 500, date: date 2000 11 21 }
    -- , { id: 3, name: "E3", salary: 500, date: date 2000 12 22 }
    ]
    $ selectFrom employees \r → do
        restrict $ not $ r.date .> (litPG $ date 2000 11 21)
        pure r

  testWith ctx unordered "extract month from employees"
    [ { y: 2000, m: 10, d: 20 }
    , { y: 2000, m: 11, d: 21 }
    , { y: 2000, m: 12, d: 22 }
    ]
    $ selectFrom employees \r → do
        let y = extract "year" r.date
        let m = extract "month" r.date
        let d = extract "day" r.date
        pure { y, m, d }

  testWith ctx unordered "select * from keyword table"
    ([] ∷ Array ({ end ∷ Int }))
    $ selectFrom pgKeywordTable pure

main ∷ (TestSuite → Aff Unit) → Aff Unit
main cont = do
  pool ← liftEffect $ PostgreSQL.newPool dbconfig
  PostgreSQL.withConnection pool case _ of
    Left pgError → failure ("PostgreSQL connection error: " <> unsafeStringify pgError)
    Right conn → do
      createdb ← PostgreSQL.execute conn (PostgreSQL.Query """
        DROP TABLE IF EXISTS people;
        CREATE TABLE people (
          id INTEGER PRIMARY KEY,
          name TEXT NOT NULL,
          age INTEGER
        );

        DO $$
        BEGIN
          IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'account_type') THEN
            CREATE TYPE ACCOUNT_TYPE as ENUM (
              'business',
              'personal'
            );
          END IF;
        END$$;

        DROP TABLE IF EXISTS bank_accounts;
        CREATE TABLE bank_accounts (
          id INTEGER PRIMARY KEY,
          personId INTEGER NOT NULL,
          balance INTEGER NOT NULL,
          accountType ACCOUNT_TYPE NOT NULL
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

        DROP TABLE IF EXISTS pg_keyword_table;
        CREATE TABLE pg_keyword_table (
          "end" INTEGER NOT NULL
        );

        DROP TABLE IF EXISTS employees;
        CREATE TABLE employees (
          id SERIAL PRIMARY KEY,
          name TEXT NOT NULL,
          salary INTEGER DEFAULT 500,
          date DATE NOT NULL DEFAULT '2000-10-20'
        );
      """) PostgreSQL.Row0
      when (isJust createdb) $
        failure ("PostgreSQL createdb error: " <> unsafeStringify createdb)

      runSeldaAff conn do
        insert_ people
          [ { id: 1, name: "name1", age: Just 11 }
          , { id: 2, name: "name2", age: Just 22 }
          , { id: 3, name: "name3", age: Just 33 }
          ]
        insert_ bankAccounts
          [ { id: 1, personId: 1, balance: 100, accountType: Business }
          , { id: 2, personId: 1, balance: 150, accountType: Personal }
          , { id: 3, personId: 3, balance: 300, accountType: Personal }
          ]
        insert_ descriptions
          [ { id: 1, text: Just "text1" }
          , { id: 3, text: Nothing }
          ]
        -- id is Auto, so it cannot be inserted
        -- insert_ employees [{ id: 1, name: "E1", salary: 123 }]
        insert_ employees [{ name: "E1", salary: 123 }]
        insert1_ employees { name: "E2", date: date 2000 11 21 }
        insert1_ employees { name: "E3" }

      -- simple test delete
      runSeldaAff conn do
        insert1_ people { id: 4, name: "delete", age: Just 999 }
        deleteFrom people \r → r.id .== lit 4

      -- simple test update
      runSeldaAff conn do
        { name, age } ← insert1 people { id: 5, name: "update", age: Just 999 }
        update people
          (\r → r.name .== lit name)
          (\r → r { age = lit $ Just 1000 })
        deleteFrom people \r → r.age .> lit age

        update employees
          (\r → r.name .== lit "E3")
          (\r → r { date = litPG $ date 2000 12 22 })

      -- test a table with SQL keyword as a column name
      runSeldaAff conn do
        insert1_ pgKeywordTable_quote { "\"end\"": 1 }
        update pgKeywordTable_quote
          (\r → r."\"end\"" .== lit 1)
          (\r → r { "\"end\"" = lit 2} )
        deleteFrom pgKeywordTable_quote
          (\r → r."\"end\"" .== lit 2)

      runSeldaAff conn do
        _ ← insert people ([] ∷ Array { id ∷ Int, name ∷ String, age ∷ Maybe Int })
        insert_ people ([] ∷ Array { id ∷ Int, name ∷ String, age ∷ Maybe Int })

      cont do
        suite "PG" $ testWithPG conn legacySuite
        suite "PG.Specific" $ testWithPG conn testSuite

dbconfig ∷ PoolConfiguration
dbconfig = (defaultPoolConfiguration "purspg")
  { user = Just "init"
  , password = Just $ "qwerty"
  , idleTimeoutMillis = Just $ 1000
  }
