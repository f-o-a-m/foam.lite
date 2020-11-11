{ name = "foam.lite-helper"
, dependencies =
  [ "argonaut-generic"
  , "affjax"
  , "chanterelle"
  , "console"
  , "effect"
  , "numbers"
  , "optparse"
  , "psci-support"
  , "web3"
  ]
, packages = ./packages.dhall
, sources = [ "helper/**/*.purs", "dapp/**/*.purs" ]
}
