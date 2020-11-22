{ name = "foam.lite-app"
, dependencies =
  [ "aff-bus", "console", "effect", "halogen", "psci-support", "web3", "react", "web-mercator", "deck-gl", "react-map-gl", "js-timers", "numbers" ]
, packages = ./packages.dhall
, sources = [ "dapp/Contracts/**/*.purs", "app/**/*.purs", "dapp/DApp/Message.purs" ]
}
