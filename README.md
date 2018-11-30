# purescript-selda

## Credits

Supported by [Lambda Terms](https://github.com/lambdaterms/)

Inspired by [selda](https://github.com/valderman/selda)

## TODO
- check:
  - maybe (maybe a) ? with left join
  - literal in group by
- explicit having
- typeclass for record validation - better type errors
- more tests
- more restrict operators
- add restrictions to operators (Ord? Eq?)
- IN / NOT IN
- order by
- limit
- default, sequenced primary key - do not insert those, but do select

## Known issues
- leftJoin before select
- nested records not working - generic part - pure $ { r1: { a, b }, r2: { c }, d }
