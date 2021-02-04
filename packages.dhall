{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let override =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { "package-name" =
       mkPackage
         [ "dependency1"
         , "dependency2"
         ]
         "https://example.com/path/to/git/repo.git"
         "tag ('v4.0.0') or branch ('master')"
  , "package-name" =
       mkPackage
         [ "dependency1"
         , "dependency2"
         ]
         "https://example.com/path/to/git/repo.git"
         "tag ('v4.0.0') or branch ('master')"
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      mkPackage
        [ "arrays"
        , "exists"
        , "profunctor"
        , "strings"
        , "quickcheck"
        , "lcg"
        , "transformers"
        , "foldable-traversable"
        , "exceptions"
        , "node-fs"
        , "node-buffer"
        , "node-readline"
        , "datetime"
        , "now"
        ]
        "https://github.com/hdgarrood/purescript-benchotron.git"
        "v7.0.0"
  }
-------------------------------
-}

let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190725/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20210118/packages.dhall sha256:a59c5c93a68d5d066f3815a89f398bcf00e130a51cb185b2da29b20e2d8ae115

let overrides = {=}

let polyform =
      { dependencies =
          [ "debug"
          , "foreign"
          , "foreign-object"
          , "generics-rep"
          , "invariant"
          , "newtype"
          , "ordered-collections"
          , "parsing"
          , "psci-support"
          , "profunctor"
          , "quickcheck-laws"
          , "run"
          , "test-unit"
          , "transformers"
          , "validation"
          , "variant"
          ]
      , repo =
          "https://github.com/purescript-polyform/polyform.git"
      , version =
          "master"
      }

let polyform-batteries =
      { dependencies =
          [ "affjax"
          , "argonaut"
          , "debug"
          , "decimals"
          , "filterable"
          , "numbers"
          , "polyform"
          , "prelude"
          , "record-extra"
          , "test-unit"
          ]
      , repo =
          "https://github.com/purescript-polyform/batteries.git"
      , version =
          "master"
      }

let polyform-batteries-env =
      { dependencies =
          [ "polyform-batteries" ]
      , repo =
          "https://github.com/purescript-polyform/batteries-env.git"
      , version =
          "master"
      }

let postgresql-client =
      { dependencies =
          [ "aff"
          , "arrays"
          , "bifunctors"
          , "bytestrings"
          , "datetime"
          , "decimals"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign"
          , "foreign-generic"
          , "foreign-object"
          , "js-date"
          , "lists"
          , "maybe"
          , "newtype"
          , "nullable"
          , "prelude"
          , "string-parsers"
          , "transformers"
          , "tuples"
          ]
      , repo =
          "https://github.com/rightfold/purescript-postgresql-client.git"
      , version =
          "pool-query"
      }

let additions =
      { postgresql-client =
          postgresql-client
      , polyform =
          polyform
      , polyform-batteries =
          polyform-batteries
      , polyform-batteries-env =
          polyform-batteries-env
      }

in  upstream // overrides // additions
