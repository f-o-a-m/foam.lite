{ name = "foam.lite-server"
, dependencies =
  [ "argonaut-generic"
  , "chanterelle"
  , "console"
  , "effect"
  , "node-net"
  , "nodetrout"
  , "numbers"
  , "psci-support"
  , "web3"
  ]
, packages = ./packages.dhall
, sources = [ "server/**/*.purs", "dapp/**/*.purs" ]
}
