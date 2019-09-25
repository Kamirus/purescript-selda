# Simple End-to-End Example

```purescipt
module Main where

import Prelude

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Database.PostgreSQL as PostgreSQL
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Class.Console (log, logShow)
import Selda (class MonadSelda, Col, FullQuery, Table(..), aggregate, count, groupBy, insert_, leftJoin, lit, notNull, query, restrict, selectFrom, selectFrom_, showQuery, (.==), (.>))
```

First we have to setup the database.
We create a db called *purspg* and a user.
We include this information in a record below.
We will need it later to execute our queries.

```purescript
dbconfig ∷ PostgreSQL.PoolConfiguration
dbconfig = (PostgreSQL.defaultPoolConfiguration "purspg")
  { user = Just "init"
  , password = Just $ "qwerty"
  , idleTimeoutMillis = Just $ 1000
  }
```

Now we should create some tables in our database.
We will use the `postgresql-client` to get the job done.
To do so we define a auxiliary function `createTable` that takes the SQL string literal with table definition, executes it and simply throws an error if something went wrong.

```purescript
createTable ∷ String → PostgreSQL.Connection → Aff Unit
createTable sql conn = do
  PostgreSQL.execute conn (PostgreSQL.Query sql) PostgreSQL.Row0
    >>= maybe (pure unit) (throwError <<< error <<< show)
```

The function that creates our first table - `people` - is defined below.

```purescript
createPeople ∷ PostgreSQL.Connection → Aff Unit
createPeople = createTable """
  DROP TABLE IF EXISTS people;
  CREATE TABLE people (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    age INTEGER
  );"""
```

To represent this table definition in PureScript, we use a `Table` type which is parameterized by a *row of types* that describes the columns and their types from the database.

Please note that `purescript-selda` does not handle schema modification, such as table creation, we are doing it manually using `postgresql-client`.
So it is important to correctly define the tables and types for its columns.

Notice that `name` column has `NOT NULL` constraint, unlike the `age` column.
We use `Maybe` for type of nullable columns.

```purescript
people ∷ Table (id ∷ Int, name ∷ String, age ∷ Maybe Int)
people = Table { name: "people" }
```

Similarly we create second table - `bankAccounts`.

```purescript
createBankAccounts ∷ PostgreSQL.Connection → Aff Unit
createBankAccounts = createTable """
  DROP TABLE IF EXISTS bank_accounts;
  CREATE TABLE bank_accounts (
    id INTEGER PRIMARY KEY,
    personId INTEGER NOT NULL,
    balance INTEGER NOT NULL
  );"""

bankAccounts ∷ Table ( personId ∷ Int, id ∷ Int, balance ∷ Int)
bankAccounts = Table { name: "bank_accounts" }
```

Since we have defined table definitions, we can write some queries.
Let's start with a simple `LEFT JOIN` query with some arbitrary condition in the `WHERE` clause.

```purescript
-- Why is it worth to create queries of FullQuery type? to reuse in other queries
-- simple query with left join, some arbitrary where with restrict
qNamesWithBalance
  ∷ ∀ s. FullQuery s { name ∷ Col s String , balance ∷ Col s (Maybe Int) }
qNamesWithBalance = 
  selectFrom people \{ id, name, age } → do
    { balance } ← leftJoin bankAccounts \acc → id .== acc.personId
    restrict $ id .> lit 1
    pure { name, balance }
```

Before we explain every operation used let's look how the generated SQL for this query looks like

```sql
  SELECT people_0.name AS name, bank_accounts_1.balance AS balance
  FROM people people_0
  LEFT JOIN bank_accounts bank_accounts_1 ON ((people_0.id = bank_accounts_1.personId))
  WHERE (people_0.id > 1)
```

We define a query using the `selectFrom` function.
It takes two arguments: a table definition and a function that takes a record of table's columns and returns a query description.
Operations such as `restrict` and `leftJoin` modify the state of the query.
For example: the `leftJoin` call in the query above adds the `LEFT JOIN` clause.
At the end we pick which columns should be included in the result.

Notice that `leftJoin` also changes the types in a column's record. The `balance` column is nullable in that context, so it has type `Maybe Int`.

We can use the previously defined `qNamesWithBalance` as a subquery to filter out the null values in the `balance` column.

```purescript
-- we can reuse other queries to use them as subqueries
-- notNull changes the balance type, filtering only results with Just values
qBankAccountOwnersWithBalance
  ∷ ∀ s. FullQuery s { name ∷ Col s String , balance ∷ Col s Int }
qBankAccountOwnersWithBalance = 
  selectFrom_ qNamesWithBalance \{ name, balance: nullableBalance } → do
    balance ← notNull nullableBalance
    pure { name, balance }
```



```purescript
qCountBankAccountOwners
  ∷ ∀ s. FullQuery s { numberOfOwners ∷ Col s String }
qCountBankAccountOwners = 
  aggregate $ selectFrom_
    (aggregate $ selectFrom bankAccounts \{ id, personId } → do
      pid ← groupBy personId
      pure { pid })
    \{ pid } → pure { numberOfOwners: count pid }

app ∷ ∀ m. MonadSelda m ⇒ m Unit
app = do
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

  log $ showQuery qNamesWithBalance
  query qNamesWithBalance >>= logShow

  log $ showQuery qBankAccountOwnersWithBalance
  query qBankAccountOwnersWithBalance >>= logShow

  log $ showQuery qCountBankAccountOwners
  query qCountBankAccountOwners >>= logShow

main ∷ Effect Unit
main = do
  pool ← PostgreSQL.newPool dbconfig
  launchAff_ do
    PostgreSQL.withConnection pool case _ of
      Left pgError → logShow ("PostgreSQL connection error: " <> show pgError)
      Right conn → do
        createPeople conn
        createBankAccounts conn

        runReaderT (runExceptT app) conn >>= either logShow pure
```
