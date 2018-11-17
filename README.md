# WIP

# TODO

- simple joins
- git branch cleanup
- structure cleanup, renames and restrict what modules export
- more restrict operators
- add restrictions to operators (Ord? Eq?)
- subqueries:
  - aggregate
  - IN / NOT IN
- order by
- limit
- default, sequenced primary key - do not insert those, but do select

# Known issues
- leftJoin before select
- nested records not working - generic part - pure $ { r1: { a, b }, r2: { c }, d }
