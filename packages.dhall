let mkPackage =
  https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
  https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221201/packages.dhall
    sha256:d1a68fa15709eaa686515eb5b9950d82c743f7bf73e3d87a4abe9e1be6fda571

in upstream
  with postgresql-client =
    mkPackage
      [ "aff", "argonaut", "arrays", "assert", "bifunctors", "bytestrings"
      , "datetime", "decimals", "dotenv", "effect", "either", "enums"
      , "exceptions", "foldable-traversable", "foreign", "foreign-generic"
      , "foreign-object", "identity", "integers", "js-date", "lists", "maybe"
      , "newtype", "node-process", "nullable", "ordered-collections", "partial"
      , "polyform", "polyform-batteries-core", "polyform-batteries-env", "prelude"
      , "psci-support", "string-parsers", "strings", "test-unit", "transformers"
      , "tuples", "typelevel-prelude", "validation"
      ]
      "https://github.com/rightfold/purescript-postgresql-client.git"
      "v3.4.1"
    with
      polyform =
        mkPackage
        [ "heterogeneous", "js-unsafe-stringify", "newtype" ,"ordered-collections"
        , "variant", "profunctor", "invariant", "foreign-object"
        , "run", "transformers","validation", "foreign"
        ]
        "https://github.com/purescript-polyform/polyform.git"
        "v0.9.2"

    with
      polyform-batteries-core = mkPackage
        [ "debug", "decimals", "filterable", "numbers"
        , "polyform", "prelude", "record-extra", "test-unit"
        ]
        "https://github.com/purescript-polyform/batteries-core.git"
        "v0.3.0"
    with
      polyform-batteries-urlencoded =
        mkPackage
          [ "argonaut" , "console" , "debug" , "effect" , "form-urlencoded"
          , "polyform-batteries-core" , "psci-support" , "spec"
          ]
          "https://github.com/purescript-polyform/batteries-urlencoded.git"
          "v0.4.1"
    with polyform-batteries-env =
      mkPackage
        [ "arrays"
        , "identity"
        , "maybe"
        , "ordered-collections"
        , "polyform"
        , "polyform-batteries-core"
        , "prelude"
        , "psci-support"
        , "typelevel-prelude"
        ]
        "https://github.com/purescript-polyform/batteries-env.git"
        "v0.2.0"
    with bytestrings =
      mkPackage
        [ "arrays", "console", "effect", "exceptions", "foldable-traversable"
        , "integers", "leibniz", "maybe", "newtype", "node-buffer", "partial"
        , "prelude", "quickcheck", "quickcheck-laws", "quotient", "unsafe-coerce"
        ]
        "https://github.com/martyall/purescript-bytestrings.git"
        "e51cf868a4137c1c48c98d32115bb2014c9f7624"
    with quotient =
      mkPackage
      [ "prelude", "quickcheck" ]
      "https://github.com/rightfold/purescript-quotient.git"
      "v3.0.0"
    with foreign-generic =
      mkPackage
        [ "arrays", "assert", "bifunctors", "console", "control"
        , "effect", "either", "exceptions", "foldable-traversable"
        , "foreign", "foreign-object", "identity", "lists", "maybe"
        , "newtype", "partial", "prelude", "record", "strings"
        , "transformers", "tuples", "unsafe-coerce"
        ]
        "https://github.com/paluh/purescript-foreign-generic.git"
        "a5c23d29e72619624978446293ac9bb45ccd2fde"
    with js-unsafe-stringify =
      mkPackage
        ([] : List Text)
        "https://github.com/paluh/purescript-js-unsafe-stringify.git"
        "master"
