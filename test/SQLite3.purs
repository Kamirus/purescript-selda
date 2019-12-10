module Test.SQLite3 where

import Prelude

import Data.Foldable (for_)
import Effect.Aff (Aff)
import SQLite3 (newDB, queryDB)
import Test.Common (legacySuite)
import Test.Unit (TestSuite, suite)
import Test.Utils (testWithSQLite3)

main ∷ (TestSuite → Aff Unit) → Aff Unit
main cont = do
  let dbPath = "./test/db.sqlite3"
  conn ← newDB dbPath
  
  -- recreate tables
  for_ strsCreateTables \s → queryDB conn s []
  
  -- inserts
  _ ← queryDB conn """
    INSERT INTO people (id, name, age)
    VALUES
      (1,'name1',11),
      (2,'name2',22),
      (3,'name3',33);
    """ []
  _ ← queryDB conn """
    INSERT INTO bank_accounts (id, personId, balance, accountType)
    VALUES
      (1, 1, 100, 'business'),
      (2, 1, 150, 'personal'),
      (3, 3, 300, 'personal');
    """ []
  _ ← queryDB conn """
    INSERT INTO descriptions (id, text)
    VALUES
      (1, 'text1'),
      (3, null);
    """ []
  
  -- test
  -- queryDB conn "SELECT id FROM people WHERE 10 > 2" [] >>= (log <<< unsafeStringify)
  -- queryDB conn "SELECT sum(id), count(id), max(id) FROM emptyTable" [] >>= (log <<< unsafeStringify)
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
