# Custom Types

- [Custom Types](#custom-types)
  - [Preface](#preface)
  - [Before](#before)
  - [New Custom Type](#new-custom-type)
    - [Table Definition](#table-definition)
    - [`FromSQLValue`](#fromsqlvalue)
    - [`ToSQLValue`](#tosqlvalue)
      - [`litPG` vs `lit`](#litpg-vs-lit)
  - [Instances](#instances)
    - [Main Execution](#main-execution)
      - [Output](#output)

## Preface

This cook book recipe is a literate PureScript file, which is designed to be a standalone runnable example.

This guide describes how to handle custom data types *as-types-of-columns* in selda - how to define tables with them, query values of these types and write queries with these values as parameters.

```purescript
module CookBook.CustomTypes where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Database.PostgreSQL (class FromSQLValue, class ToSQLValue)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, logShow)
import Foreign (readString, unsafeToForeign)
import Global.Unsafe (unsafeStringify)
import Guide.SimpleE2E as Guide
import Selda (Col, FullQuery, Table(..), distinct, innerJoin, innerJoin_, lit, notNull, restrict, selectFrom, showQuery, (.==), (.>=))
import Selda.Col (class GetCols)
import Selda.PG (litPG, showPG)
import Selda.PG.Class (insert_)
import Selda.PG.Class as PG
import Selda.Query.Class (runSelda)
```
## Before

Selda supports simple data types like String, Int, Maybe on its own.
We can simply declare a table definition using these types.

*(Remember that selda does not modify the database schema -
a table definition is like a type annotation for already created table)*

```purescript
people ∷ Table ( name ∷ String, age ∷ Maybe Int, id ∷ Int )
people = Table { name: "people" }
```

also querying values of these simple types and 
lifting them to column expressions `Col s a` (using the function `lit ∷ a → Col s a`)
works just fine.

```purescript
selectAdults
  ∷ ∀ s
  . FullQuery s { name ∷ Col s String, age ∷ Col s (Maybe Int), id ∷ Col s Int }
selectAdults = selectFrom people \r → do
  age ← notNull r.age
  restrict $ age .>= lit 18
  pure r

queryAdults
  ∷ ∀ m
  . PG.MonadSeldaPG m
  ⇒ m (Array { name ∷ String , age ∷ Maybe Int , id ∷ Int })
queryAdults = PG.query selectAdults
```

## New Custom Type

But we sometimes want to use other data types.
Say we want to define another table called `bankAccounts` with a column
called `accountType` that is an enum with possible values:
"personal" or "business"

It's convenient to mirror it with a custom data type `AccountType` in PureScript

```purescript
data AccountType
  = Business
  | Personal
derive instance eqAccountType ∷ Eq AccountType
derive instance genericAccountType ∷ Generic AccountType _
instance showAccountTyp ∷ Show AccountType where
  show = genericShow
```

### Table Definition

We use `AccountType` to annotate a column `accountType` in the following table definition

```purescript
bankAccounts ∷ Table ( personId ∷ Int, balance ∷ Int, accountType ∷ AccountType )
bankAccounts = Table { name: "bank_accounts" }
```

As an example let's consider a query that returns distinct account types of 
a person with a given name

```purescript
selectAccountTypesOf
  ∷ ∀ s
  . String
  → FullQuery s { accountType ∷ Col s AccountType }
selectAccountTypesOf name = distinct $ selectFrom people \r → do
  { accountType } ← innerJoin bankAccounts \b → b.personId .== r.id
  restrict $ r.name .== lit name
  pure { accountType }
```

But when we try to execute this query we get the following error:

  ```
  No type class instance was found for
  Database.PostgreSQL.Value.FromSQLValue AccountType
  ```

Since we are using `PG.query` function for PostgreSQL we get an
error specific to the `purescript-postgresql-client`.
It uses two type classes (`ToSQLValue` and `FromSQLValue`) to
handle serialization and deserialization.

### `FromSQLValue`

`FromSQLValue` is used to parse `Foreign` value into `AccountType` in this case
There are already provided instances for common data types, but we need to
write our own to handle querying `AccountType` 

```purescript
queryAccountTypesOf
  ∷ ∀ m
  . PG.MonadSeldaPG m
  ⇒ FromSQLValue AccountType -- just to emphasise that we need this instance
  ⇒ m (Array { accountType ∷ AccountType })
queryAccountTypesOf = PG.query $ selectAccountTypesOf "John Smith"
```

### `ToSQLValue`

Similarly we would like to write queries with `AccountType` values.
For example we could restrict a query to personal accounts only.
Thus we want a function of type `AccountType → Col s AccountType` that could
lift a value to a column expression.
Previously we used `lit` to do that but it is restricted to simple data types.
Here we need to use PostgreSQL specific function `litPG` that can create
a column expression `Col s a` provided that there's an instance of `ToSQLValue a`

#### `litPG` vs `lit`

> Side note: One could use `litPG` exclusively and don't bother with dilemma when to use `lit` and when to use `litPG`.
> But then every query becomes PG specific and might break when executed by another (e.g. SQLite3) backend.
> Another difference between `lit` and `litPG` is that `lit` serializes a value to a string so it is visible in the printed query.
> `litPG` on the other hand makes a query parameter - it serializes a value to `Foreign` and inserts a placeholder where `lit` might write a string.

```purescript
selectAdultAccounts
  ∷ ∀ s
  . ToSQLValue AccountType -- just to emphasise that we need this instance
  ⇒ FullQuery s
      { balance ∷ Col s Int
      , id ∷ Col s Int
      }
selectAdultAccounts = selectFrom bankAccounts \r@{balance} → do
  adult ← innerJoin_ (\a → a.id .== r.personId) selectAdults
  restrict $ r.accountType .== litPG Personal -- Only personal accounts!
  pure { id: adult.id, balance }
```

## Instances

Now to discharge these constraints we need instances of `FromSQLValue` and `ToSQLValue` for `AccountType`.

```purescript
instance fromSqlValueAccountType ∷ FromSQLValue AccountType where
  fromSQLValue = readAccountType <=< lmap show <<< runExcept <<< readString

readAccountType ∷ String → Either String AccountType
readAccountType "business" = Right Business
readAccountType "personal" = Right Personal
readAccountType  other = Left $ "Incorrect account type: " <> other

instance toSQLValueProductType ∷ ToSQLValue AccountType where
  toSQLValue = showAccountType >>> unsafeToForeign

showAccountType ∷ AccountType → String
showAccountType Business = "business"
showAccountType Personal = "personal"
```

To sum up - to handle custom data types all we need to do is write appriopriate instances for the backend we chose.
For pgclient they are: `ToSQLValue` and `FromSQLValue`.

To make it complete let's execute queries that we've covered.
We will use some functionality from the [Guide](SimpleE2E.md).

To execute the main function below plese run following commands:
  ```
  npm run-script lit
  spago run -m CookBook.CustomTypes
  ```

### Main Execution

```purescript
main ∷ Effect Unit
main = Guide.launchWithConnectionPG \conn → do
  -- create tables
  Guide.createPeople conn
  Guide.execute -- create bank accounts
    """
    DROP TABLE IF EXISTS bank_accounts;
    CREATE TABLE bank_accounts (
      id SERIAL PRIMARY KEY,
      personId INTEGER NOT NULL,
      balance INTEGER NOT NULL DEFAULT 100,
      accountType TEXT NOT NULL
    );""" conn

  runSelda conn app >>= either logShow pure

app ∷ ∀ m. PG.MonadSeldaPG m ⇒ m Unit
app = do
  -- insert some rows
  insert_ people
    [ { id: 1, name: "Just Mark", age: Just 11 }
    , { id: 2, name: "John Smith", age: Just 22 }
    ]
  insert_ bankAccounts
    [ { personId: 1, balance: 100, accountType: Personal }
    , { personId: 2, balance: 1000, accountType: Personal }
    , { personId: 2, balance: 1000, accountType: Business }
    , { personId: 2, balance: 2341, accountType: Business }
    ]
  logQuery $ selectAdults
  logQuery $ selectAccountTypesOf "John Smith"
  logQuery $ selectAdultAccounts

  log "query results"
  logShow =<< queryAdults
  logShow =<< queryAccountTypesOf
  logShow =<< PG.query selectAdultAccounts

logQuery ∷ ∀ s i m. GetCols i ⇒ MonadEffect m ⇒ FullQuery s { | i } → m Unit
logQuery q = do
  let { strQuery, params } = showPG $ showQuery q
  log strQuery
  log $ unsafeStringify params
  log ""
```

#### Output
(may be outdated)
  ```
 SELECT people_0.name AS name, people_0.id AS id, people_0.age AS age
 FROM people people_0
 WHERE (people_0.age >= 18) AND (people_0.age IS NOT NULL)
[]

 SELECT DISTINCT bank_accounts_1.accountType AS accountType
 FROM people people_0
 JOIN bank_accounts bank_accounts_1 ON ((bank_accounts_1.personId = people_0.id))
 WHERE (people_0.name = 'John Smith')
[]

 SELECT sub_q1.id AS id, bank_accounts_0.balance AS balance
 FROM bank_accounts bank_accounts_0
 JOIN 
  ( SELECT people_0.name AS name, people_0.id AS id, people_0.age AS age
    FROM people people_0
    WHERE (people_0.age >= 18) AND (people_0.age IS NOT NULL) ) sub_q1 ON ((sub_q1.id = bank_accounts_0.personId))
 WHERE (bank_accounts_0.accountType = $1)
["personal"]

query results
[{ age: (Just 22), id: 2, name: "John Smith" }]
[{ accountType: Business },{ accountType: Personal }]
[{ balance: 1000, id: 2 }]
  ```
