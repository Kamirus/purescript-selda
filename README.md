# purescript-selda

## Work In Progress

## About

**purescript-selda** is a **SQL library** which allows a user to write **type-safe queries**.
Generated SQL is guaranteed to be correct by the type system.
It supports arbitrarily nested queries with capabilities of filtering, joins and aggregation.
We used standard monadic abstraction which supports writing queries in a linear, natural style.

**[More information with an introduction, please refer to my thesis on this subject.](./selda.pdf)**

## Examples

We assume the database is setup and tables are created. 

First we have to provide description for these tables: **table name, column names and their types.**

Consider the following SQL table definition:

```sql
  CREATE TABLE people (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    age INTEGER
  );
```

To represent this table definition in PureScript, we use a `Table` type which is parameterized by a *row of types* that describes the columns and their types from the database.

```purescript
  people ∷ Table (id ∷ Int, name ∷ String, age ∷ Maybe Int)
  people = Table { name: "people" }
```

Just to show how a query can look like, we include a simple example together with a generated SQL for this query.

```purescript
selectFrom people \{ id, name, age } → do
  { balance } ← leftJoin bankAccounts \b → id .== b.personId
  restrict $ id .> lit 1
  pure { id, balance }
```

```sql
SELECT people_0.id AS id, bank_accounts_1.balance AS balance
FROM people people_0
LEFT JOIN bank_accounts bank_accounts_1
  ON ((people_0.id = bank_accounts_1.personId))
WHERE ((people_0.id > 1))
```

More examples in [my thesis](./selda.pdf) and `test/Main.purs`.

## More info

For information about features, examples, usage, etc. refer to the test suite in `test/Main.purs`.

## Credits

Supported by [Lambda Terms](https://github.com/lambdaterms/)

Inspired by [selda](https://github.com/valderman/selda)
