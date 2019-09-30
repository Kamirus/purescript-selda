{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "purescript-selda"
, dependencies =
    [ "console"
    , "exists"
    , "heterogeneous"
    , "postgresql-client"
    , "prelude"
    , "strings"
    , "transformers"
    , "test-unit"
    , "variant"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
