{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "foam.lite-lora"
, dependencies =
  [ "argonaut"
  , "argonaut-generic"
  , "array-views"
  , "b64"
  , "chanterelle"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "integers"
  , "maybe"
  , "node-buffer"
  , "numbers"
  , "parsing"
  , "parsing-dataview"
  , "prelude"
  , "psci-support"
  , "purescript-node-datagram"
  , "strings"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "lora/**/*.purs", "dapp/**/*.purs" ]
}
