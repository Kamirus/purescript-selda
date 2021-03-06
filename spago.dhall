{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "selda"
, license =
    "MIT"
, repository =
    "https://github.com/Kamirus/purescript-selda.git"
, dependencies =
    [ "console"
    , "exists"
    , "heterogeneous"
    , "lists"
    , "node-sqlite3"
    , "postgresql-client"
    , "prelude"
    , "simple-json"
    , "strings"
    , "test-unit"
    , "transformers"
    , "variant"
    , "prettyprinter"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs", "guide/src/**/*.purs" ]
}
