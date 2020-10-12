module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import UI.Component.Table as Table



main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Table.component unit body
