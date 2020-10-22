module DApp.Deploy.Main (main) where
  
import Prelude

import Chanterelle.Deploy (deployWithProvider)
import DApp.Deploy.LedgerSupport (ledgerHttpProvider, stopLedgerProvider)
import DApp.Deploy.Script (deployScript)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Network.Ethereum.Web3 (httpProvider)
import Node.Process (lookupEnv)

main :: Effect Unit
main = launchAff_ do
  let runWithProvider p = void $ deployWithProvider p 60 deployScript
  nodeUrl <- fromMaybe "http://localhost:8545" <$> liftEffect (lookupEnv "NODE_URL")
  ledgerNetwork <- liftEffect $ lookupEnv "LEDGER_NETWORK_ID"
  case ledgerNetwork of
    Nothing -> (liftEffect $ httpProvider nodeUrl) >>= runWithProvider
    Just ledgerNetworkString -> do
      networkId <- maybe (throwError $ error "couldn't parse LEDGER_NETWORK_ID") pure (Int.fromString ledgerNetworkString)
      let ledgerDefaultDerivationPath = "44'/60'/0'/0"
      ledgerPath <- fromMaybe ledgerDefaultDerivationPath <$> liftEffect (lookupEnv "LEDGER_HD_PATH")
      let ledgerOptions = { networkId
                          , path: ledgerPath 
                          , askConfirm: true
                          , accountsLength: 1
                          , accountsOffset: 0
                          }
      provider <- liftEffect $ ledgerHttpProvider nodeUrl ledgerOptions
      runWithProvider provider
      liftEffect $ stopLedgerProvider provider