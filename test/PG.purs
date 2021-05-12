module Test.PG where

import Prelude

import Data.Date (Date, canonicalDate)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Database.PostgreSQL (Connection, PoolConfiguration, defaultPoolConfiguration)
import Database.PostgreSQL as PostgreSQL
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Selda (Col, Table(..), S, lit, restrict, selectFrom, (.==), (.>))
import Selda.PG (extract, generateSeries, litPG)
import Selda.PG.Class (BackendPGClass, deleteFrom, insert, insert1, insert1_, insert_, update)
import Selda.Table.Constraint (Auto, Default)
import Test.Common (bankAccounts, descriptions, legacySuite, people)
import Test.Types (AccountType(..))
import Test.Unit (TestSuite, failure, suite)
import Test.Utils (PGSelda, TestCtx, assertSeqEq, assertUnorderedSeqEq, runSeldaAff, testWith, testWithPG, unsafeStringify)

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
pgKeywordTable_quote = Table { name: "pg_keyword_table" }

qualifiedTableWithSchema ∷ Table ( id ∷ Auto Int, name ∷ String )
qualifiedTableWithSchema = Source "tablename"
  \alias → "qualified.tablename" <> maybe "" (" " <> _) alias

date ∷ Int → Int → Int → Date
date y m d = unsafePartial $ fromJust $
  canonicalDate <$> toEnum y <*> toEnum m <*> toEnum d

testSuite
  ∷ TestCtx BackendPGClass PGSelda { conn ∷ Connection }
  → TestSuite
testSuite ctx = do
  let
    unordered = assertUnorderedSeqEq
    -- Note: ordered is not being used. not sure if this is
    -- an unfinished test or not, so commenting out for now
    -- ordered = assertSeqEq

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
        let (y ∷ Col S _) = extract "year" r.date
        let m = extract "month" r.date
        let d = extract "day" r.date
        pure { y, m, d }

  testWith ctx unordered "select * from keyword table"
    ([] ∷ Array ({ end ∷ Int }))
    $ selectFrom pgKeywordTable pure

  testWith ctx unordered "select * from qualifiedTableWithSchema"
    [{ id: 2, name: "s2" }]
    $ selectFrom qualifiedTableWithSchema pure

  testWith ctx unordered "generate_series(3,5)"
    [ { i: 3 }
    , { i: 4 }
    , { i: 5 }
    ]
    $ selectFrom (generateSeries 3 5) pure

main ∷ (TestSuite → Aff S) → Aff S
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

        DROP TABLE IF EXISTS qualified.tablename;
        DROP SCHEMA IF EXISTS qualified;
        CREATE SCHEMA qualified;
        CREATE TABLE qualified.tablename (
          id SERIAL PRIMARY KEY,
          name TEXT NOT NULL
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

        insert_ qualifiedTableWithSchema
          [ { name: "s1" }
          , { name: "s2" }
          ]
        update qualifiedTableWithSchema
          (\r → r.id .== lit 1)
          (\r → r { name = lit "s" })
        deleteFrom qualifiedTableWithSchema
          (\r → r.name .== lit "s")

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
