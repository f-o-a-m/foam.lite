module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import UI.Component.Root as Root
import UI.Config (makeAppEnv)
import UI.Monad (runAppM)

main :: Effect Unit
main = do 
  Console.log "Starting App..."
  environment <- makeAppEnv
  HA.runHalogenAff do
    body <- HA.awaitBody
    let component = H.hoist (runAppM environment) Root.component
    runUI component unit body
