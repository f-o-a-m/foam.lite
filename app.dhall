{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "foam5g"
, dependencies = [ "console", "effect", "psci-support", "web3" ]
, packages = ./packages.dhall
, sources = [ "dapp/Contracts/**/*.purs", "app/**/*.purs"]
}
