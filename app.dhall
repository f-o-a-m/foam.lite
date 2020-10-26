{ name = "foam5g-app"
, dependencies =
  [ "console", "effect", "halogen", "ocelot", "psci-support", "web3", "react", "web-mercator", "deck-gl", "react-map-gl" ]
, packages = ./packages.dhall
, sources = [ "dapp/Contracts/**/*.purs", "app/**/*.purs" ]
}
