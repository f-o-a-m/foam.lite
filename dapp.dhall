{ name = "foam5g-dapp"
, dependencies = [ "console", "effect", "numbers", "psci-support", "chanterelle", "web3" ]
, packages = ./packages.dhall
, sources = [ "dapp/**/*.purs"]
}
