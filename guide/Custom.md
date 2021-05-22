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
  - [Unsafe Escape Hatch](#unsafe-escape-hatch)
    - [`litPG` and `EForeign`](#litpg-and-eforeign)
    - [Any](#any)
      - [Custom PG function](#custom-pg-function)
    - [Table](#table)
      - [Table-like Source](#table-like-source)
        - [DB Schema - qualified table names](#db-schema---qualified-table-names)
        - [generate_series](#generate_series)
  - [Summary](#summary)
    - [Main Execution](#main-execution)
      - [Output](#output)

## Preface

This cook book recipe is a literate PureScript file, which is designed to be a standalone runnable example.

This guide describes how to handle custom data types *as-types-of-columns* in selda - how to define tables with them, query values of these types and write queries with these values as parameters.

```purescript
module Guide.Custom where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Database.PostgreSQL (class FromSQLValue, class ToSQLValue)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, logShow)
import Foreign (readString, unsafeToForeign)
import Guide.SimpleE2E as Guide
import Selda (Col(..), FullQuery, Table(..), distinct, innerJoin, innerJoin_, lit, notNull, restrict, selectFrom, showQuery, (.==), (.>=))
import Selda.Col (class GetCols, showCol)
import Selda.Expr (Expr(..))
import Selda.PG (litPG, showPG)
import Selda.PG.Class (insert_)
import Selda.PG.Class as PG
import Selda.Query.Class (runSelda)

-- necessary since Global.Unsafe (unsafeStringify) was removed
foreign import unsafeStringify :: forall a. a -> String
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

## Unsafe Escape Hatch

Sometimes we want to make unsafe extensions and expose them via safe interface.
One of such extensions would be to write a piece of SQL in a string and integrate it with selda expressions.

Selda has fixed ADT that represent expressions (e.g. literals, some binary operations, some aggregate functions) - but it has two escape hatches that allow a user to make extensions.

Firstly, user can provide a `Foreign` value (which is utilised by `litPG`).

Secondly, user can provide an SQL in a string (covered below - [Any](#any))

### `litPG` and `EForeign`

It is worth knowing the implementation of `litPG`.

  ```purescript
  litPG ∷ ∀ col s a. ToSQLValue a ⇒ Coerce col ⇒ a → col s a
  litPG = unsafeFromCol <<< Col <<< EForeign <<< toSQLValue
  ```

- `toSQLValue` serializes a value to `Foreign`
- `Col <<< EForeign` is an escape hatch - it creates a `Col s a` for any `Foreign` value, thus it is worth to use it carefully and always provide a type annotation
- `unsafeFromCol` is a method from the `Coerce` class

> `class Coerce` : 
> allows overloading for values that can be both `Col s a` and `Aggr s a`.
> 
> For example: columns from a table need to be in the `GROUP BY` clause to be safely coerced to `Aggr`, while constants can be safely used in both contexts.
> Hence we can use `litPG` for writing `restrict` (`Col s Boolean`) and `having` (`Aggr s Boolean`) conditions.

### Any

Second use case: user can provide any SQL string as an expression.
It is possible with the following function

  ```purescript
  (Col <<< Any) ∷ ∀ s a. ShowM → Col s a
  ```

It is best to think of `ShowM` as an abstract monad `m String` that additionally supports `showCol ∷ ∀ s a. Col s a → ShowM`.

To simply provide an SQL string use `(Col <<< Any <<< pure) ∷ ∀ s a. String → Col s a`

#### Custom PG function

But it is usually not enough to use raw Strings.
Some expressions depend on others, meaning we would have to serialize each of them to a string, but some are `Foreign` query parameters not meant to be *stringified*.
Thus we need the `ShowM` monad and `showCol` function (or any `showX` function returning `ShowM`) to make proper string representations for every `Col` expression *(returning a placeholder (e.g. "$7") and accumulating parameters in case of `Foreign` query parameter)*

Consider the following PG-specific function [`EXTRACT`](https://www.postgresql.org/docs/current/functions-datetime.html#FUNCTIONS-DATETIME-EXTRACT) that retrieves subfields from date/time values.

We would like to encode it in selda.
Say we want its type to be `extract ∷ ∀ a s. String → Col s a → Col s Int`.

***Please note** it is possibe to write a **more type-safe variant** either by restricting `a` to a date type or not allowing arbitrary strings as its first argument.
But we want to keep it simple.
User can write a safer alternative by wrapping the function `extract` defined below.*

Let's implement it:

```purescript
extract ∷ ∀ a s. String → Col s a → Col s Int
extract field col = Col $ Any do
  s ← showCol col
  pure $ "extract(" <> field <> " from " <> s <> ")"
```

- `field` could be one of `["day", "month", "year"]` 
- `extract` depends on another expression `Col s a` so we need to turn in into a string using `showCol` function in order to use `Any`
- In the last line we build a raw SQL string for the whole expression
- We annotated it to return `Col s Int` because we know that it would match the foreign value returned one the query is executed - We could annotate it differently and maybe provide our own custom data type and handle \[de\]serialization ourselves with `From/ToSQLValue` instances.

> **SQL expressions vs. SQL statements**
> 
> One could think that now we can write any query as a raw SQL string since we have `Any`, but it is not that simple.
> We can only use it to represent expressions whereas query is a `SELECT` statement, though there are expressions that depend on statements e.g. `IN` takes an expression and a query and returns a bool so it is expressible using `Any` (see: `in_` function).
>
> Though it is possible to encode more than just expressions - please see [Table-like Source](#table-like-source)

### Table

To represent database tables we normally use `Table` constructor to provide the table name as well as column names with their corresponding types.

Say we have a table named `"users"` with columns `( name ∷ String, id ∷ Int )`.
When we generate SQL for a query involving `users` each column is prefixed with an alias for its *source* (here the *source* is the table `users`).

Aliases are created using the table/source name and a unique number.
- Meaning instead of `name` in a generated SQL there is `users_7.name`.
- and after `FROM` (or `JOIN` ...) we see `... FROM users users_7 ...`

These are the only two uses for `Table` data type during SQL generation:
1. get an alias so column names can be qualified
2. get its representation - a string that should appear after `FROM` (or `JOIN` ...)

**Problem:**
- tables created in different database schemas are not definable with the `Table` constructor (for a table `"myschema.users"` it produces incorrect qualified column names like `"myschema.users_3.name"`)
- set returning database functions (like [`generate_series`](https://www.postgresql.org/docs/current/functions-srf.html)) that could be treated like a read-only tables yield a similar problem with aliases

#### Table-like Source

To retain readable aliases and fix problems above (and open more possibilities) there's another constructor called `Source` for the `Table` data type.

`Source ∷ ∀ r. Alias → (Maybe Alias → StringSQL) → Table r`

`Alias` and `StringSQL` are just aliases for `String`.
So to create an arbitrary table-like source we need:
- an alias - a prefix of the full alias used to qualify column names (a unique number will be provided during SQL generation)
- a way to create its string-representation given a full alias (with a unique number already concatenated)
  - in case of `INSERT`/`UPDATE`/`DELETE` there's no alias hence there's `Maybe`

##### DB Schema - qualified table names 

To represent tables with schema-qualified names we use the `Source` constructor.

**Example**: Given a table called `"myschema.users"` we create a table definition for it in a following way.

```purescript
myschemaUsers ∷ Table ( name ∷ String, id ∷ Int )
myschemaUsers = Source "users" $ case _ of
  Nothing    → "myschema.users"
  Just alias → "myschema.users" <> " " <> alias
```

Notice that the type-level part is the same as for the `Table` constructor - meaning we define columns the same way as before.

##### generate_series

Similarly we utilise the `Source` constructor to represent set returning functions that can be treated like read-only tables, but aren't really tables.

```purescript
generateSeries ∷ Int → Int → Table ( i ∷ Int )
generateSeries start stop = Source "gs" \maybeAlias →
  let alias = maybe "" identity maybeAlias in
  "generate_series(" <> show start <> ", " <> show stop <> ") " <> alias <> " (i)"
```

- We ignore alias being `Nothing` (it will break when we attempt to call insert/update/delete on it).
- The string returned by the function in `Source` is tied to the type-level information - name of the column `i` is provided in the string as well as in the type row

Executing a query `selectFrom (generateSeries 3 5) pure` the following SQL is generated

  ```SQL
  SELECT gs_0.i AS i
  FROM generate_series(3, 5) gs_0 (i)
  ```

## Summary

To sum up - to handle custom data types all we need to do is write appriopriate instances for the backend we chose.
For pgclient they are: `ToSQLValue` and `FromSQLValue`.

To make it complete let's execute queries that we've covered.
We will use some functionality from the [Guide](SimpleE2E.md).

To execute the main function below plese run following commands:
  ```
  npm run-script lit && spago run -m Guide.Custom
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
      "personId" INTEGER NOT NULL,
      balance INTEGER NOT NULL DEFAULT 100,
      "accountType" TEXT NOT NULL
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
