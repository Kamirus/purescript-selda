# purescript-selda

## About

**purescript-selda** is an **SQL library** (*eDSL*) which allows a user to write **type-safe queries**.
- Generated SQL is guaranteed to be correct by the type system.
- It supports **arbitrarily nested queries** with capabilities of **filtering**, **joins** and **aggregation**.
- We used **standard monadic abstraction** which supports writing queries in a linear, natural style.
- Our main target is **PostgreSQL** though in the upcoming release (already on master) we add **SQLite3** support (with ability to support other db backends).


## Example Query

To declare a type for a SQL table (already created in the db)
we write the following table definition:
```purescript
people ∷ Table (id ∷ Int, name ∷ String, age ∷ Maybe Int)
people = Table { name: "people" }
```

Once we've defined the tables, we can write queries, e.g.

```purescript
selectFrom people \{ id, name, age } → do
  { balance } ← leftJoin bankAccounts \b → id .== b.personId
  restrict $ id .> lit 1
  pure { id, balance }
```

Generated SQL for the above query:
```sql
SELECT people_0.id AS id, bank_accounts_1.balance AS balance
FROM people people_0
LEFT JOIN bank_accounts bank_accounts_1 ON ((people_0.id = bank_accounts_1.personId))
WHERE ((people_0.id > 1))
```

For a more gentle introduction and more examples please refer to the [Step-by-Step Guide](guide/SimpleE2E.md).

## More Help

**If you have any questions please don't hesitate to ask**.
<br>I'll be happy to help and provide any guidance if necessary.
<br>Open an issue or hit me up directly (either on [slack](https://functionalprogramming.slack.com/), [forum](https://discourse.purescript.org/) or directly via email).

## Install

Install [postgresql-client's dependencies](https://github.com/rightfold/purescript-postgresql-client#install)
> npm install pg decimal.js

## Info

- **Guide**: [Introductory End-to-End example that shows how to setup, write queries, use aggregation, deal with type errors and execute queries and inserts.](guide/SimpleE2E.md)
- **Test Suite**: For information about features, examples, usage, etc. refer to the test suite in `test/Main.purs`.
- **Documentation**: [Pursuit docs](https://pursuit.purescript.org/packages/purescript-selda/)
- [**My thesis**](./selda.pdf)

## Credits

Supported by [Lambda Terms](https://github.com/lambdaterms/)

Inspired by [selda](https://github.com/valderman/selda)
