module Test.Common where

import Prelude

import Data.Maybe (Maybe(..))
import Selda (Col, FullQuery, Table(..), aggregate, count, crossJoin, desc, distinct, groupBy, inArray, isNull, leftJoin, leftJoin_, limit, lit, max_, not_, orderBy, restrict, selectFrom, selectFrom_, sum_, (.&&), (.==), (.>), (.||))
import Selda.PG (litF)
import Selda.Query (notNull)
import Selda.Query.Class (class GenericQuery)
import Test.Types (AccountType(..))
import Test.Unit (TestSuite)
import Test.Utils (class TestBackend, TestCtx, assertSeqEq, assertUnorderedSeqEq, testWith)

people ∷ Table ( name ∷ String , age ∷ Maybe Int , id ∷ Int )
people = Table { name: "people" }

bankAccounts ∷ Table ( personId ∷ Int, id ∷ Int, balance ∷ Int, accountType ∷ AccountType )
bankAccounts = Table { name: "bank_accounts" }

descriptions ∷ Table ( id ∷ Int, text ∷ Maybe String )
descriptions = Table { name: "descriptions" }

emptyTable ∷ Table ( id ∷ Int )
emptyTable = Table { name: "emptyTable" }

testSelectEscapedString
  ∷ ∀ b m ctx s
  . TestBackend b m ctx
  ⇒ GenericQuery b m s
      ( val ∷ Col s String )
      ( val ∷ String )
  ⇒ TestCtx b m s ctx
  → TestSuite
testSelectEscapedString ctx = do
  testWith ctx assertUnorderedSeqEq "select escaped string"
    [ { val: "'abc' \' \"def\"" } ]
    $ aux
  where
    aux ∷ FullQuery s { val ∷ Col s String }
    aux = selectFrom people \r → do
      restrict $ r.id .== lit 1
      pure { val: lit "'abc' \' \"def\"" }

legacySuite ctx = do
  let
    unordered = assertUnorderedSeqEq
    ordered = assertSeqEq

  testSelectEscapedString ctx

  testWith ctx unordered "simple select people"
    [ { id: 1, name: "name1", age: Just 11 }
    , { id: 2, name: "name2", age: Just 22 }
    , { id: 3, name: "name3", age: Just 33 }
    ]
    $ selectFrom people \r → do
        pure r

  testWith ctx unordered "select people, return different record"
    [ { x: 1, y: Just 11 }
    , { x: 2, y: Just 22 }
    , { x: 3, y: Just 33 }
    ]
    $ selectFrom people \{ id, age } → do
        pure { x: id, y: age }

  testWith ctx unordered "simple select people restrict"
    [ { id: 2, name: "name2", age: Just 22 }
    , { id: 3, name: "name3", age: Just 33 }
    ]
    $ selectFrom people \r@{ age } → do
        restrict $ age .> lit (Just 20)
        pure r

  testWith ctx unordered "simple select restrict on custom type"
    [ { id: 2, personId: 1, balance: 150, accountType: Personal }
    , { id: 3, personId: 3, balance: 300, accountType: Personal }
    ]
    $ selectFrom bankAccounts \r@{ accountType } → do
        restrict $ accountType .== litF Personal
        pure r

  testWith ctx unordered "cross product with restrict"
    [ { id1: 2, age1: Just 22, age2: Just 11 }
    , { id1: 3, age1: Just 33, age2: Just 11 }
    , { id1: 3, age1: Just 33, age2: Just 22 }
    ]
    $ selectFrom people \r1 → do
        r2 ← crossJoin people
        restrict $ r1.age .> r2.age
        pure { id1: r1.id, age1: r1.age, age2: r2.age }

  testWith ctx unordered "leftJoin: just Maybe Int insead of Maybe Maybe Int "
    [ { id: 1
      -- , age1: Just 11, age2: Just $ Just 11
      , age1: Just 11, age2: Just 11
      , name1: "name1", name2: Just "name1" }
    ]
    $ selectFrom people \r1 → do
        r2 ← leftJoin people \{ id } → id .== r1.id
        restrict $ r1.id .== lit 1
        -- error
        -- orderBy asc $ max_ r1.id
        pure
          { id: r1.id
          , age1: r1.age, age2: r2.age
          , name1: r1.name, name2: r2.name }

  testWith ctx unordered "leftJoin maybe column: Just Nothing vs Nothing"
    [ { id: 1, text: Just "text1" }
    , { id: 2, text: Nothing }
    , { id: 3, text: Nothing }
    -- , { id: 3, text: Just Nothing }
    ]
    $ selectFrom people \r → do
        { text } ← leftJoin descriptions \{ id } → r.id .== id
        pure { id: r.id, text }

  testWith ctx unordered "cross product as natural join"
    [ { id: 1, balance: 100, accountType: Business }
    , { id: 1, balance: 150, accountType: Personal }
    -- , { id: 2, balance: Nothing }
    , { id: 3, balance: 300, accountType: Personal }
    ]
    $ selectFrom people \{ id, name, age } → do
        { accountType, balance, personId } ← crossJoin bankAccounts
        restrict $ id .== personId
        pure { accountType, id, balance }

  testWith ctx unordered "left join"
    [ { id: 1, balance: Just 100 }
    , { id: 1, balance: Just 150 }
    , { id: 2, balance: Nothing }
    , { id: 3, balance: Just 300 }
    ]
    $ selectFrom people \{ id, name, age } → do
        { balance } ← leftJoin bankAccounts \b → id .== b.personId
        pure { id, balance }

  testWith ctx unordered "left join but with subquery"
    [ { id: 1, balance: Just 100 }
    , { id: 1, balance: Just 150 }
    , { id: 2, balance: Nothing }
    , { id: 3, balance: Just 300 }
    ]
    $ selectFrom people \{ id, name, age } → do
        { balance } ← leftJoin_ (\b → id .== b.personId) do
          selectFrom bankAccounts \b → do
            -- restrict $ id .== b.personId -- type error
            pure b
        pure { id, balance }

  testWith ctx unordered "subquery with aggregate max"
    [ { balance: Just 150, id: 1 }
    , { balance: Nothing, id: 2 }
    , { balance: Just 300, id: 3 }
    ] $ selectFrom people \{ id, name, age } -> do
          { balance } <- leftJoin_ (\b -> id .== b.personId) $
            aggregate $ selectFrom bankAccounts \b -> do
              personId <- groupBy b.personId
              -- restrict $ id .> lit 1
              pure { personId, balance: max_ b.balance }
          pure { id, balance }

  testWith ctx unordered "aggr: max people id"
    [ { maxId: Just 3 } ]
    $ aggregate $ selectFrom people \{ id, name, age } → do
        pure { maxId: max_ id }

  testWith ctx unordered "aggr: max people id from bankAccounts with counts"
    [ { pid: 1, m: Just 150, c: 2 }
    , { pid: 3, m: Just 300, c: 1 }
    ]
    $ aggregate $ selectFrom bankAccounts \{ personId, balance } → do
        pid ← groupBy personId
        pure { pid, m: max_ balance, c: count personId }

  testWith ctx ordered "aggr: order by max people id desc"
    [ { pid: 3, m: 300, c: 1 }
    , { pid: 1, m: 150, c: 2 }
    ]
    $ selectFrom_ do
        aggregate $ selectFrom bankAccounts \{ personId, balance } → do
            pid ← groupBy personId
            pure { pid, m: max_ balance, c: count personId }
        $ \r@{ pid, c } → do
            m ← notNull r.m
            orderBy desc m
            pure { pid, m, c }

  testWith ctx unordered "aggr: max people id having count > 1"
    [ { pid: 1, m: 150, c: 2 }
    ]
    $ selectFrom_ do
        aggregate $ selectFrom bankAccounts \{ personId, balance } → do
            pid ← groupBy personId
            pure { pid, m: max_ balance, c: count personId }
        $ \r@{ pid, c } → do
            m ← notNull r.m
            restrict $ c .> lit 1
            pure { pid, m, c }

  testWith ctx unordered "limit negative returns 0"
    [ ]
    $ selectFrom people \r → do
        limit $ -7
        pure r

  testWith ctx unordered "limit + order by: return first"
    [ { pid: 3, maxBalance: Just 300 } ]
    $ aggregate $ selectFrom bankAccounts \{ personId, balance } → do
        pid ← groupBy personId
        limit 1
        orderBy desc personId
        pure { pid, maxBalance: max_ balance }

  testWith ctx unordered "max(id) on empty table returns 1 result: null"
    [ { maxId: Nothing } ]
    $ aggregate $ selectFrom emptyTable \r → pure { maxId: max_ r.id }

  testWith ctx unordered "max(id) on empty table returns 0 results with notNull"
    ([ ] ∷ Array { id ∷ Int })
    $ selectFrom_ do
        aggregate $ selectFrom emptyTable \r →
            pure { maxId: max_ r.id }
        $ \r → do
            id ← notNull r.maxId
            pure { id }

  testWith ctx unordered "sum(balance); OR operator"
    [ { pid: 1, sum: Just 250 }
    , { pid: 2, sum: Nothing }
    ]
    $ aggregate $ selectFrom people \p → do
        b ← leftJoin bankAccounts \{ personId } → p.id .== personId
        pid ← groupBy p.id
        restrict $ p.id .== lit 1 .|| p.id .== lit 2
        pure { pid, sum: sum_ b.balance}

  testWith ctx unordered "return only not null values"
    [ { id: 1, text: "text1" } ]
    $ selectFrom descriptions \ { id, text: maybeText } → do
        text ← notNull maybeText
        pure { id, text }

  testWith ctx unordered "inArray"
    [ { id: 1, name: "name1", age: Just 11 }
    , { id: 3, name: "name3", age: Just 33 }
    ]
    $ selectFrom people \r → do
        restrict $ r.id `inArray` [ lit 1, lit 3 ]
        pure r

  testWith ctx unordered "not inArray"
    [ { id: 2, name: "name2", age: Just 22 } ]
    $ selectFrom people \r → do
        restrict $ not_ $ r.id `inArray` [ lit 1, lit 3 ]
        pure r

  testWith ctx unordered "select distinct personId from bankAccounts"
    [ { pid: 1 }
    , { pid: 3 }
    ]
    $ distinct $ selectFrom bankAccounts \r → do
        pure { pid: r.personId }

  testWith ctx unordered "restricted inner join without a subquery"
    [ { pid: 1 }
    , { pid: 3 }
    ]
    $ selectFrom people \r → do
        b ← leftJoin bankAccounts
          (\b → r.id .== b.personId .&& b.balance .> lit 100)
        _ ← notNull b.id 
        pure { pid: r.id }

  testWith ctx unordered "select not null text from descriptions"
    [ { id: 3, text: Nothing } ]
    $ selectFrom descriptions \r → do
        restrict $ isNull r.text
        pure r
