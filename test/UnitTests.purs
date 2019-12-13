module Test.UnitTests where

import Prelude

import Selda (Table(..))
import Selda.Query.ShowStatement (genericShowInsert, mkPlaceholders)
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assertEq)

testTable ∷ Table ( c ∷ Int, a ∷ String, z ∷ Int )
testTable = Table { name: "testTable" }

testSuite ∷ TestSuite
testSuite = suite "Unit" do

  test "mkPlaceholders" do
    assertEq
      (mkPlaceholders "$" 1 3 2)
      "($1, $2, $3), ($4, $5, $6)"
    assertEq
      (mkPlaceholders "$" 1 3 0)
      ""

  test "genericShowInsert" do
    let ctx = { ph: "$" }
    assertEq
      (genericShowInsert ctx testTable [{ c: 1, a: "a1", z: 11}])
      "INSERT INTO testTable (z, c, a) VALUES ($1, $2, $3);"
    assertEq
      (genericShowInsert ctx testTable
        [ { c: 1, a: "a1", z: 11}
        , { c: 2, a: "a2", z: 22}
        ])
      "INSERT INTO testTable (z, c, a) VALUES ($1, $2, $3), ($4, $5, $6);"
