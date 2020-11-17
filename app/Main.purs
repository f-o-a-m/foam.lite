module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import UI.Component.Root as Root
import UI.Config (makeAppEnv)
import UI.Monad (runAppM)
import Web.DOM.ChildNode (remove) as WD
import Web.DOM.ParentNode (QuerySelector(..))  as WD
import Web.HTML.HTMLElement as WHH

main :: Effect Unit
main = do 
  Console.log "Starting App..."
  environment <- makeAppEnv
  HA.runHalogenAff do
    body <- awaitBodyAndCleanupAppRootIfExists
    let component = H.hoist (runAppM environment) Root.component
    runUI component unit body

  -- useful for webpack hotreloads where it seems like halogen just appends the app root
  -- wherever you put it, instead of replacing the existing one
  where awaitBodyAndCleanupAppRootIfExists = do
          -- wait for <body>
          body <- HA.awaitBody

          -- cleanup our <div id="app-root"> created in Root.component
          appRoot <- HA.selectElement (WD.QuerySelector "#app-root")
          case appRoot of
            Nothing -> pure unit
            Just e -> liftEffect $ WD.remove (WHH.toChildNode e)

          -- clean up MapboxGL's mapboxgl-map dummy element that gets created
          mapboxMap <- HA.selectElement (WD.QuerySelector ".mapboxgl-map")
          case mapboxMap of
            Nothing -> pure unit
            Just e -> liftEffect $ WD.remove (WHH.toChildNode e)
          
          -- return <body>
          pure body
