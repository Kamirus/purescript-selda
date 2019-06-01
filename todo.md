
## TODO
- check:
  - insert_ T [{ id: 1...}]; insert_ T [{ ... }] 2nd without id; key exists!
  - table name: public.Table, alias without public...
  - literal in group by - runtime error
  - add additional phantom type for Col to indicate OR change Expr to polymorphic variant
- explicit having
- typeclass for record validation - better type errors
- more tests
- more restrict operators
- add restrictions to operators (Ord? Eq?)
- IN / NOT IN
- order by
- limit
- default, sequenced primary key - do not insert those, but do select
- error handling
- delete and update with RETURNING
- pgclient stable version
- notNull (:: Maybe a -> a)
- print query, query to string

## Known issues
- max on empty set returns null (workaround: avoid aggregations with group by () on empty tables)
- orderBy in a query without groupBy (also orderBy max sth that is not in the group by)
- groupBy not a column, results in runtime error
- nested records not working - generic part - pure $ { r1: { a, b }, r2: { c }, d }
