{ name = "foam5g-app"
, dependencies =
  [ "console", "effect", "halogen", "ocelot", "psci-support", "web3" ]
, packages = ./packages.dhall
, sources = [ "dapp/Contracts/**/*.purs", "app/**/*.purs" ]
}
