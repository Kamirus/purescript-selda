module Test.SQLite3 where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import SQLite3 (newDB, queryDB)
import Selda (lit, (.==), (.>))
import Selda.SQLite3.Class (deleteFrom, insert_, update)
import Test.Common (bankAccounts, descriptions, legacySuite, people)
import Test.Types (AccountType(..))
import Test.Unit (TestSuite, suite)
import Test.Utils (runSeldaAff, testWithSQLite3)

main ∷ (TestSuite → Aff Unit) → Aff Unit
main cont = do
  let dbPath = "./test/db.sqlite3"
  conn ← newDB dbPath
  
  -- recreate tables
  for_ strsCreateTables \s → queryDB conn s []
  
  -- inserts
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

  runSeldaAff conn do
    insert_ people ([] ∷ Array { id ∷ Int, name ∷ String, age ∷ Maybe Int })

  cont do
    suite "SQLite3" $ testWithSQLite3 conn legacySuite

strsCreateTables ∷ Array String
strsCreateTables =
  [ "DROP TABLE IF EXISTS people;"
  , """
    CREATE TABLE people (
      id INTEGER PRIMARY KEY,
      name TEXT NOT NULL,
      age INTEGER
    );
    """
  , "DROP TABLE IF EXISTS bank_accounts;"
  , """
    CREATE TABLE bank_accounts (
      id INTEGER PRIMARY KEY,
      personId INTEGER NOT NULL,
      balance INTEGER NOT NULL,
      accountType TEXT NOT NULL
    );
    """
  , "DROP TABLE IF EXISTS descriptions;"
  , """
    CREATE TABLE descriptions (
      id INTEGER PRIMARY KEY,
      text TEXT
    );
    """
  , "DROP TABLE IF EXISTS emptyTable;"
  , """
    CREATE TABLE emptyTable (
      id INTEGER PRIMARY KEY
    );
    """
  ]
