{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "parsing"
  , "matryoshka"
  , "dodo-printer"
  , "strings"
  , "strings-extra"
  , "generics-rep"
  , "debug"
  , "test-unit"
  , "quickcheck"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
