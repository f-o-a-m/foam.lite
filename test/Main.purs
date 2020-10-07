module Test.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import DApp.Deploy.Script (deployScript)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Minutes(..), fromDuration)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Process (lookupEnv)
import Spec.DApp.Common (testConfigToSpecConfig)
import Spec.DApp.FungibleToken (fungibleTokenSpec) as DAppSpecs
import Spec.DApp.RelayableNFT (relayableNFTSpec) as DAppSpecs
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

main :: Effect Unit
main = launchAff_ do
  nodeUrl <- liftEffect $ fromMaybe "http://localhost:8545" <$> lookupEnv "NODE_URL"
  testConfig <- buildTestConfig nodeUrl 60 deployScript
  let specConfig = testConfigToSpecConfig testConfig
  runSpec' defaultConfig {timeout = Just (fromDuration $ Minutes 20.0) } [consoleReporter] do
    DAppSpecs.fungibleTokenSpec specConfig
    DAppSpecs.relayableNFTSpec specConfig