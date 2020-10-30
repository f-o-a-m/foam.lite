module UI.Config where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Network.Ethereum.Core.Signatures (Address, mkAddress)
import Network.Ethereum.Web3 (Provider, httpProvider, mkHexString)
import Partial.Unsafe (unsafeCrashWith)

foreign import providerURLImpl :: String

foreign import relayableNFTImpl :: String

type Contracts = 
  { relayableNFT :: Address
  }

newtype AppEnv = AppEnv
  { web3Provider :: Provider
  , contracts :: Contracts
  }

makeAppEnv :: Effect AppEnv
makeAppEnv = do
  log relayableNFTImpl
  case mkHexString relayableNFTImpl >>= mkAddress of
    Nothing -> unsafeCrashWith $ "Unable to parse RelayableNFT Address:" <> relayableNFTImpl
    Just relayableNFT -> do 
      web3Provider <- httpProvider "http://localhost:8545"
      pure $ AppEnv 
        { web3Provider
        , contracts: {relayableNFT}
        }
