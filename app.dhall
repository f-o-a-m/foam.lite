{ name = "foam5g-app"
, dependencies =
  [ "aff-bus", "console", "effect", "halogen", "ocelot", "psci-support", "web3", "react", "web-mercator", "deck-gl", "react-map-gl" ]
, packages = ./packages.dhall
, sources = [ "dapp/Contracts/**/*.purs", "app/**/*.purs", "dapp/DApp/Message.purs" ]
}
