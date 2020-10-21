module Test.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import DApp.Deploy.Script (deployScript)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Minutes(..), fromDuration)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Network.Ethereum.Core.Signatures (generatePrivateKey)
import Node.Process (lookupEnv)
import Spec.DApp.Common (testConfigToSpecConfig)
import Spec.DApp.FungibleToken (fungibleTokenSpec) as DAppSpecs
import Spec.DApp.Message (messageSpec) as DAppSpecs
import Spec.DApp.Relay (relaySpec) as DAppSpecs
import Spec.DApp.RelayableNFT (relayableNFTSpec) as DAppSpecs
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = launchAff_ do
  nodeUrl <- liftEffect $ fromMaybe "http://localhost:8545" <$> lookupEnv "NODE_URL"
  testConfig <- buildTestConfig nodeUrl 60 deployScript
  privateKey <- liftEffect generatePrivateKey
  let specConfig = testConfigToSpecConfig testConfig privateKey
  Console.log $ unsafeCoerce specConfig
  runSpec' defaultConfig {timeout = Just (fromDuration $ Minutes 20.0) } [consoleReporter] do
    DAppSpecs.messageSpec
    DAppSpecs.relaySpec
    DAppSpecs.fungibleTokenSpec specConfig
    DAppSpecs.relayableNFTSpec specConfig
