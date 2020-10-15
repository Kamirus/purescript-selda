{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "selda"
, license = "MIT"
, repository = "https://github.com/Kamirus/purescript-selda.git"
, dependencies =
  [ "console"
  , "debug"
  , "dotenv"
  , "exists"
  , "heterogeneous"
  , "lists"
  , "node-sqlite3"
  , "polyform-batteries-env"
  , "postgresql-client"
  , "prelude"
  , "prettyprinter"
  , "simple-json"
  , "strings"
  , "test-unit"
  , "transformers"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "guide/src/**/*.purs" ]
}
