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
    , "postgresql-client"
    , "prelude"
    , "strings"
    , "test-unit"
    , "transformers"
    , "variant"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs", "guide/src/**/*.purs" ]
}
