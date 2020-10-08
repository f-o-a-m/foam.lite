module Test.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import DApp.Deploy.Script (deployScript)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Time.Duration (Minutes(..), fromDuration)
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Network.Ethereum.Core.HexString (HexString)
import Network.Ethereum.Core.Signatures (mkPrivateKey)
import Node.Process (lookupEnv)
import Spec.DApp.Common (testConfigToSpecConfig)
import Spec.DApp.FungibleToken (fungibleTokenSpec) as DAppSpecs
import Spec.DApp.Message (messageSpec) as DAppSpecs
import Spec.DApp.RelayableNFT (relayableNFTSpec) as DAppSpecs
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

foreign import generatePrivateKey :: forall m. Monad m => (HexString -> m HexString) -> m HexString

main :: Effect Unit
main = launchAff_ do
  nodeUrl <- liftEffect $ fromMaybe "http://localhost:8545" <$> lookupEnv "NODE_URL"
  testConfig <- buildTestConfig nodeUrl 60 deployScript
  privateKey' <- generatePrivateKey pure
  privateKey <- maybe (throwError $ error "Couldn't generate a private key outside of web3!") pure $ mkPrivateKey privateKey'
  let specConfig = testConfigToSpecConfig testConfig privateKey
  runSpec' defaultConfig {timeout = Just (fromDuration $ Minutes 20.0) } [consoleReporter] do
    DAppSpecs.messageSpec
    DAppSpecs.fungibleTokenSpec specConfig
    DAppSpecs.relayableNFTSpec specConfig