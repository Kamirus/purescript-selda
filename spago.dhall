{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "selda"
, license = "MIT"
, repository = "https://github.com/Kamirus/purescript-selda.git"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "exists"
  , "foldable-traversable"
  , "foreign"
  , "heterogeneous"
  , "leibniz"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-sqlite3"
  , "partial"
  , "postgresql-client"
  , "prelude"
  , "prettyprinter"
  , "record"
  , "simple-json"
  , "strings"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "guide/src/**/*.purs" ]
}
