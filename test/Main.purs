module Test.Main where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.Date (Date, canonicalDate)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Variant.Internal (FProxy(..))
import Database.PostgreSQL (Connection, PGError, PoolConfiguration, defaultPoolConfiguration)
import Database.PostgreSQL as PostgreSQL
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Global.Unsafe (unsafeStringify)
import Guide.SimpleE2E as Guide.SimpleE2E
import Partial.Unsafe (unsafePartial)
import SQLite3 (DBConnection)
import Selda (Col, FullQuery, Table(..), aggregate, count, crossJoin, desc, groupBy, inArray, leftJoin, leftJoin_, limit, lit, max_, not_, orderBy, restrict, selectFrom, selectFrom_, sum_, (.==), (.>), (.||))
import Selda.PG (litF)
import Selda.PG.Class (BackendPGClass, deleteFrom, insert1_, insert_, update)
import Selda.Query (notNull)
import Selda.Query.Class (class GenericQuery, genericQuery)
import Selda.Table.Constraint (Auto, Default)
import Test.Types (AccountType(..))
import Test.Unit (TestSuite, failure, suite)
import Test.Unit.Main (runTest)
import Test.Utils (PGSelda, assertSeqEq, assertUnorderedSeqEq, runSeldaAff, testQueryWith_)
import Type.Proxy (Proxy(..))

people ∷ Table ( name ∷ String , age ∷ Maybe Int , id ∷ Int )
people = Table { name: "people" }

bankAccounts ∷ Table ( personId ∷ Int, id ∷ Int, balance ∷ Int, accountType ∷ AccountType )
bankAccounts = Table { name: "bank_accounts" }

descriptions ∷ Table ( id ∷ Int, text ∷ Maybe String )
descriptions = Table { name: "descriptions" }

emptyTable ∷ Table ( id ∷ Int )
emptyTable = Table { name: "emptyTable" }

employees ∷ Table
  ( id ∷ Auto Int
  , name ∷ String
  , salary ∷ Default Int
  , date ∷ Default Date
  )
employees = Table { name: "employees" }

date ∷ Int → Int → Int → Date
date y m d = unsafePartial $ fromJust $
  canonicalDate <$> toEnum y <*> toEnum m <*> toEnum d

main ∷ Effect Unit
main = do
  -- run literate guides
  Guide.SimpleE2E.main

  -- integration test suite
  pool ← PostgreSQL.newPool dbconfig
  void $ launchAff do
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
          insert1_ people { id: 5, name: "update", age: Just 999 }
          update people
            (\r → r.name .== lit "update")
            (\r → r { age = lit $ Just 1000 })
          deleteFrom people \r → r.age .> lit (Just 999)

          update employees
            (\r → r.name .== lit "E3")
            (\r → r { date = litF $ date 2000 12 22 })

        liftEffect $ runTest $ do
          suite "Selda" $ do
            suite "PG" $ testWithPG conn legacySuite
            -- suite "SQLite3" $ testWithSQLite3 conn legacySuite

legacySuite ctx = do
  let
    unordered = assertUnorderedSeqEq
    ordered = assertSeqEq
  testWith ctx unordered "simple select people"
    [ { id: 1, name: "name1", age: Just 11 }
    , { id: 2, name: "name2", age: Just 22 }
    , { id: 3, name: "name3", age: Just 33 }
    ]
    $ selectFrom people \r → do
        pure r

  testWith ctx unordered "select people, return different record"
    [ { x: 1, y: Just 11 }
    , { x: 2, y: Just 22 }
    , { x: 3, y: Just 33 }
    ]
    $ selectFrom people \{ id, age } → do
        pure { x: id, y: age }

  testWith ctx unordered "simple select people restrict"
    [ { id: 2, name: "name2", age: Just 22 }
    , { id: 3, name: "name3", age: Just 33 }
    ]
    $ selectFrom people \r@{ age } → do
        restrict $ age .> lit (Just 20)
        pure r

  testWith ctx unordered "simple select restrict on custom type"
    [ { id: 2, personId: 1, balance: 150, accountType: Personal }
    , { id: 3, personId: 3, balance: 300, accountType: Personal }
    ]
    $ selectFrom bankAccounts \r@{ accountType } → do
        restrict $ accountType .== lit Personal
        pure r

  testWith ctx unordered "cross product with restrict"
    [ { id1: 2, age1: Just 22, age2: Just 11 }
    , { id1: 3, age1: Just 33, age2: Just 11 }
    , { id1: 3, age1: Just 33, age2: Just 22 }
    ]
    $ selectFrom people \r1 → do
        r2 ← crossJoin people
        restrict $ r1.age .> r2.age
        pure { id1: r1.id, age1: r1.age, age2: r2.age }

  testWith ctx unordered "leftJoin: just Maybe Int insead of Maybe Maybe Int "
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

  testWith ctx unordered "leftJoin maybe column: Just Nothing vs Nothing"
    [ { id: 1, text: Just "text1" }
    , { id: 2, text: Nothing }
    , { id: 3, text: Nothing }
    -- , { id: 3, text: Just Nothing }
    ]
    $ selectFrom people \r → do
        { text } ← leftJoin descriptions \{ id } → r.id .== id
        pure { id: r.id, text }

  testWith ctx unordered "cross product as natural join"
    [ { id: 1, balance: 100, accountType: Business }
    , { id: 1, balance: 150, accountType: Personal }
    -- , { id: 2, balance: Nothing }
    , { id: 3, balance: 300, accountType: Personal }
    ]
    $ selectFrom people \{ id, name, age } → do
        { accountType, balance, personId } ← crossJoin bankAccounts
        restrict $ id .== personId
        pure { accountType, id, balance }

  testWith ctx unordered "left join"
    [ { id: 1, balance: Just 100 }
    , { id: 1, balance: Just 150 }
    , { id: 2, balance: Nothing }
    , { id: 3, balance: Just 300 }
    ]
    $ selectFrom people \{ id, name, age } → do
        { balance } ← leftJoin bankAccounts \b → id .== b.personId
        pure { id, balance }

  testWith ctx unordered "left join but with subquery"
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

  testWith ctx unordered "subquery with aggregate max"
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

  testWith ctx unordered "aggr: max people id"
    [ { maxId: Just 3 } ]
    $ aggregate $ selectFrom people \{ id, name, age } → do
        pure { maxId: max_ id }

  testWith ctx unordered "aggr: max people id"
    [ { pid: 1, m: Just 150, c: "2" }
    , { pid: 3, m: Just 300, c: "1" }
    ]
    $ aggregate $ selectFrom bankAccounts \{ personId, balance } → do
        pid ← groupBy personId
        pure { pid, m: max_ balance, c: count personId }

  testWith ctx ordered "aggr: order by max people id desc"
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

  testWith ctx unordered "aggr: max people id having count > 1"
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

  testWith ctx unordered "limit negative returns 0"
    [ ]
    $ selectFrom people \r → do
        limit $ -7
        pure r

  testWith ctx unordered "limit + order by: return first"
    [ { pid: 3, maxBalance: Just 300 } ]
    $ aggregate $ selectFrom bankAccounts \{ personId, balance } → do
        pid ← groupBy personId
        limit 1
        orderBy desc personId
        pure { pid, maxBalance: max_ balance }

  testWith ctx unordered "max(id) on empty table returns 1 result: null"
    [ { maxId: Nothing } ]
    $ aggregate $ selectFrom emptyTable \r → pure { maxId: max_ r.id }

  testWith ctx unordered "max(id) on empty table returns 0 results with notNull"
    ([ ] ∷ Array { id ∷ Int })
    $ selectFrom_ do
        aggregate $ selectFrom emptyTable \r →
            pure { maxId: max_ r.id }
        $ \r → do
            id ← notNull r.maxId
            pure { id }

  testWith ctx unordered "sum(balance); OR operator"
    [ { pid: 1, sum: Just "250" }
    , { pid: 2, sum: Nothing }
    ]
    $ aggregate $ selectFrom people \p → do
        b ← leftJoin bankAccounts \{ personId } → p.id .== personId
        pid ← groupBy p.id
        restrict $ p.id .== lit 1 .|| p.id .== lit 2
        pure { pid, sum: sum_ b.balance}

  testWith ctx unordered "return only not null values"
    [ { id: 1, text: "text1" } ]
    $ selectFrom descriptions \ { id, text: maybeText } → do
        text ← notNull maybeText
        pure { id, text }

  testWith ctx unordered "employees inserted with default and without salary"
    [ { id: 1, name: "E1", salary: 123, date: date 2000 10 20 }
    , { id: 2, name: "E2", salary: 500, date: date 2000 11 21 }
    -- , { id: 3, name: "E3", salary: 500, date: date 2000 12 22 }
    ]
    $ selectFrom employees \r → do
        restrict $ not_ $ r.date .> (litF $ date 2000 11 21)
        pure r

  testWith ctx unordered "inArray"
    [ { id: 1, name: "name1", age: Just 11 }
    , { id: 3, name: "name3", age: Just 33 }
    ]
    $ selectFrom people \r → do
        restrict $ r.id `inArray` [ lit 1, lit 3 ]
        pure r

  testWith ctx unordered "not inArray"
    [ { id: 2, name: "name2", age: Just 22 } ]
    $ selectFrom people \r → do
        restrict $ not_ $ r.id `inArray` [ lit 1, lit 3 ]
        pure r

-- test'
--   ∷ ∀ s o i tup ol
--   . ColsToPGHandler s i tup o
--   ⇒ GetCols i ⇒ FromSQLRow tup
--   ⇒ RL.RowToList o ol ⇒ ShowRecordFields ol o ⇒ EqRecord ol o
--   ⇒ Connection → String → Array { | o } → FullQuery s { | i } → TestSuite
-- test' = testWith assertUnorderedSeqEq

-- testWith
--   ∷ ∀ s o i tup
--   . ColsToPGHandler s i tup o
--   ⇒ GetCols i ⇒ FromSQLRow tup
--   ⇒ (Array { | o } → Array { | o } → Aff Unit) 
--   → Connection
--   → String
--   → Array { | o }
--   → FullQuery s { | i }
--   → TestSuite
-- testWith assertFunc conn = 
--   testQueryWith_ (\q → runSeldaAff conn $ query q) assertFunc

type TestCtx b m s ctx =
  { b ∷ Proxy b
  , m ∷ FProxy m
  , s ∷ Proxy s
  , ctx ∷ ctx
  }

class TestBackend b m ctx | b m → ctx where
  testWith
    ∷ ∀ s i o
    . GenericQuery b m s i o
    ⇒ TestCtx b m s ctx
    → (Array { | o } → Array { | o } → Aff Unit)
    → String
    → Array { | o }
    → FullQuery s { | i }
    → TestSuite

instance testBackendPG
    ∷ TestBackend BackendPGClass
        (ExceptT PGError (ReaderT Connection Aff))
        { conn ∷ Connection }
  where
  testWith { b, m, s, ctx: { conn } } assertFn  =
    testWith_ assertFn b (runSeldaAff conn)

testWith_
  ∷ ∀ m s i o b
  . GenericQuery b m s i o
  ⇒ (Array { | o } → Array { | o } → Aff Unit)
  → Proxy b
  → (m ~> Aff)
  → String
  → Array { | o }
  → FullQuery s { | i }
  → TestSuite
testWith_ assertFn b runM = 
  testQueryWith_ (\q → runM $ genericQuery b q) assertFn

-- testSuite
--   ∷ ∀ m s b ctx
--   . TestBackend b m ctx
--   ⇒ GenericQuery b m s
--       ( age ∷ Col s (Maybe Int), id ∷ Col s Int, name ∷ Col s String )
--       ( age ∷ Maybe Int, id ∷ Int, name ∷ String )
--   ⇒ GenericQuery b m s
--       ( x ∷ Col s Int, y ∷ Col s (Maybe Int) )
--       ( x ∷ Int, y ∷ Maybe Int )
--   ⇒ TestCtx b m s ctx → Aff Unit
testSuite ctx = do
  let
    unordered = assertUnorderedSeqEq
    ordered = assertSeqEq
  liftEffect $ runTest $ do
    suite "Selda" $ do
      testWith ctx unordered "simple select people"
        [ { id: 1, name: "name1", age: Just 11 }
        , { id: 2, name: "name2", age: Just 22 }
        , { id: 3, name: "name3", age: Just 33 }
        ]
        $ selectFrom people \r → do
            pure r

      testWith ctx ordered "select people, return different record"
        [ { x: 1, y: Just 11 }
        , { x: 2, y: Just 22 }
        , { x: 3, y: Just 33 }
        ]
        $ selectFrom people \{ id, age } → do
            pure { x: id, y: age }

      testWith ctx unordered "max(id) on empty table returns 0 results with notNull"
        ([ ] ∷ Array { id ∷ Int })
        $ selectFrom_ do
            aggregate $ selectFrom emptyTable \r →
                pure { maxId: max_ r.id }
            $ \r → do
                id ← notNull r.maxId
                pure { id }

testSuitePG ∷ ∀ s. Connection → Proxy s → Aff Unit
testSuitePG conn s = testSuite { b, m, s, ctx: { conn } } 
  where
    b = (Proxy ∷ Proxy BackendPGClass)
    m = (FProxy ∷ FProxy PGSelda)

testWithPG
  ∷ ∀ a s
  . Connection
  → (TestCtx BackendPGClass PGSelda s { conn ∷ Connection } → a)
  → a
testWithPG conn k = k { b, m, s, ctx: { conn } } 
  where
    b = (Proxy ∷ Proxy BackendPGClass)
    m = (FProxy ∷ FProxy PGSelda)
    s = (Proxy ∷ Proxy s)

-- testWithSQLite3
--   ∷ ∀ a s
--   . DBConnection
--   → (TestCtx BackendPGClass PGSelda s { conn ∷ Connection } → a)
--   → a
-- testWithSQLite3 conn k = k { b, m, s, ctx: { conn } } 
--   where
--     b = (Proxy ∷ Proxy BackendPGClass)
--     m = (FProxy ∷ FProxy PGSelda)
--     s = (Proxy ∷ Proxy s)

dbconfig ∷ PoolConfiguration
dbconfig = (defaultPoolConfiguration "purspg")
  { user = Just "init"
  , password = Just $ "qwerty"
  , idleTimeoutMillis = Just $ 1000
  }
