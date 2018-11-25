# purescript-selda

## Credits

Supported by [Lambda Terms](https://github.com/lambdaterms/)

Inspired by [selda](https://github.com/valderman/selda)

## TODO

- explicit having
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
