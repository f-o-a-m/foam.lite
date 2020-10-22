module DApp.Deploy.LedgerSupport where

import Prelude (Unit)
import Effect (Effect)
import Network.Ethereum.Web3.Types.Provider (Provider)

type LedgerOptions = { networkId      :: Int
                     , path           :: String
                     , askConfirm     :: Boolean
                     , accountsLength :: Int
                     , accountsOffset :: Int
                     }
foreign import ledgerHttpProvider :: String -> LedgerOptions -> Effect Provider

foreign import stopLedgerProvider :: Provider -> Effect Unit