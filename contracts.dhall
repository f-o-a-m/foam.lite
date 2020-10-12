{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "foam5g"
, dependencies = [ "console", "effect", "numbers", "psci-support", "chanterelle", "web3" ]
, packages = ./packages.dhall
, sources = [ ".*.purs" ]
}
