module DApp.Deploy.Main (main) where
  
import Prelude

import Chanterelle.Deploy (deployWithProvider)
import DApp.Deploy.LedgerSupport (ledgerHttpProvider, stopLedgerProvider)
import DApp.Deploy.Script (deployScript)
import Data.Either (either)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Network.Ethereum.Web3 (httpProvider, runWeb3)
import Network.Ethereum.Web3.Api (net_version)
import Node.Process (lookupEnv)

main :: Effect Unit
main = launchAff_ do
  let runWithProvider p = void $ deployWithProvider p 60 deployScript
  nodeUrl <- fromMaybe "http://localhost:8545" <$> liftEffect (lookupEnv "NODE_URL")
  ledgerNetwork <- liftEffect $ lookupEnv "LEDGER_NETWORK_ID"
  ledgerAccountOffset <- liftEffect (lookupEnv "LEDGER_ACCOUNT_OFFSET") >>= case _ of
    Nothing -> pure 0
    Just ledgerAccOffsetStr ->  maybe (throwError $ error "couldn't parse LEDGER_ACCOUNT_OFFSET") pure (Int.fromString ledgerAccOffsetStr) 

  case ledgerNetwork of
    Nothing -> (liftEffect $ httpProvider nodeUrl) >>= runWithProvider
    Just ledgerNetworkString -> do
      networkId <- if String.toLower ledgerNetworkString == "auto"
                   then do
                     eNetVersion <- (liftEffect $ httpProvider nodeUrl) >>= flip runWeb3 net_version
                     mNetVersion <- either (\e -> throwError $ error $ "Couldn't look up chain ID from node: " <> show e) (pure <<< Int.fromString) eNetVersion
                     maybe (throwError $ error "couldn't parse network ID received from Node") pure mNetVersion
                   else maybe (throwError $ error "couldn't parse LEDGER_NETWORK_ID") pure (Int.fromString ledgerNetworkString)
      let ledgerDefaultDerivationPath = "44'/60'/0'/0"
      ledgerPath <- fromMaybe ledgerDefaultDerivationPath <$> liftEffect (lookupEnv "LEDGER_HD_PATH")
      let ledgerOptions = { networkId
                          , path: ledgerPath 
                          , askConfirm: true
                          , accountsLength: 1
                          , accountsOffset: ledgerAccountOffset
                          }
      provider <- liftEffect $ ledgerHttpProvider nodeUrl ledgerOptions
      runWithProvider provider
      liftEffect $ stopLedgerProvider provider