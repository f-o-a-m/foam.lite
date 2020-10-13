module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import UI.Component.RelayableNFT.Table as Table
import UI.Monad (AppEnv(..), runAppM)



main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI $ 
    H.hoist (runAppM environment) Table.component unit body
