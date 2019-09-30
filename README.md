# purescript-selda

## About

**purescript-selda** is a **SQL library** which allows a user to write **type-safe queries**.
Generated SQL is guaranteed to be correct by the type system.
It supports arbitrarily nested queries with capabilities of filtering, joins and aggregation.
We used standard monadic abstraction which supports writing queries in a linear, natural style.

## Getting Started

Install [postgresql-client's dependencies](https://github.com/rightfold/purescript-postgresql-client#install)

## Guides

1. [Introductory End-to-End example that shows how to setup, write queries, use aggregation, deal with type errors and execute queries and inserts.](guide/SimpleE2E.md)

## More info

- For information about features, examples, usage, etc. refer to the test suite in `test/Main.purs`.
- [My thesis](./selda.pdf)

## Credits

Supported by [Lambda Terms](https://github.com/lambdaterms/)

Inspired by [selda](https://github.com/valderman/selda)
