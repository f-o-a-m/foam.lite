module UI.Config where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toLower)
import Data.Traversable (elem, for)
import Effect (Effect)
import Foreign.Object as FO
import Network.Ethereum.Core.BigNumber (hexadecimal, parseBigNumber)
import Network.Ethereum.Core.Signatures (Address, mkAddress)
import Network.Ethereum.Web3 (BlockNumber(..), HexString, Provider, httpProvider, mkHexString)
import Partial.Unsafe (unsafeCrashWith)

type ForeignMaybe a = Maybe a -> (a -> Maybe a) -> Maybe a
runForeignMaybe :: forall a. ForeignMaybe a -> Maybe a
runForeignMaybe f = f Nothing Just

foreign import baseURL :: String
foreign import getFallbackHTTPProviderURL :: ForeignMaybe String
foreign import getRelayedProvider :: ForeignMaybe Provider
foreign import getRelayedContracts :: ForeignMaybe RelayedContracts

type RelayedContract = {
  networks :: FO.Object {
    address :: String,
    blockNumber :: String
  }
}
type RelayedContracts = FO.Object RelayedContract

type Contract = FO.Object { address :: Address, blockNumber :: BlockNumber }
type Contracts = 
  { relayableNFT :: Contract
  }

newtype AppEnv = AppEnv
  { maybeWeb3Provider :: Maybe Provider
  , contracts :: Contracts
  }

newtype BlockExplorer = 
  BlockExplorer { lookupAddress :: Address -> Maybe String
                , lookupBlockNumber :: BlockNumber -> Maybe String
                , lookupBlockHash :: HexString -> Maybe String
                , lookupTx :: HexString -> Maybe String
                , lookupTxLogs :: HexString -> Maybe String
                , _description :: String -- for show and eq instances
                }

instance showBlockExplorer :: Show BlockExplorer where
  show (BlockExplorer be) = be._description

instance eqBlockExplorer :: Eq BlockExplorer where
  eq be1 be2 = show be1 `eq` show be2

blockExplorerAddressLink :: Maybe BlockExplorer -> Address -> Maybe String
blockExplorerAddressLink mbe x = mbe >>= \(BlockExplorer be) -> be.lookupAddress x

blockExplorerBlockNumberLink :: Maybe BlockExplorer -> BlockNumber -> Maybe String
blockExplorerBlockNumberLink mbe x = mbe >>= \(BlockExplorer be) -> be.lookupBlockNumber x

blockExplorerBlockHashLink :: Maybe BlockExplorer -> HexString -> Maybe String
blockExplorerBlockHashLink mbe x = mbe >>= \(BlockExplorer be) -> be.lookupBlockHash x

blockExplorerTxLink :: Maybe BlockExplorer -> HexString -> Maybe String
blockExplorerTxLink mbe x = mbe >>= \(BlockExplorer be) -> be.lookupTx x

blockExplorerTxLogsLink :: Maybe BlockExplorer -> HexString -> Maybe String
blockExplorerTxLogsLink mbe x = mbe >>= \(BlockExplorer be) -> be.lookupTxLogs x

etherscanBlockExplorer :: String -> BlockExplorer
etherscanBlockExplorer baseUrl = 
  BlockExplorer
    { lookupAddress: \a -> Just (baseUrl <> "/address/" <> show a )
    , lookupBlockNumber: \bn -> Just (baseUrl <> "/block/" <> show bn)
    , lookupBlockHash: \bh -> Just (baseUrl <> "/block/" <> show bh)
    , lookupTx: \t -> Just (baseUrl <> "/tx/" <> show t)
    , lookupTxLogs: \t -> Just (baseUrl <> "/tx/" <> show t <> "#eventlog")
    , _description: "Etherscan at " <> baseUrl
    }

networkIDMeta :: String -> Maybe { friendlyName :: String, blockExplorer :: Maybe BlockExplorer, includeInToast :: Boolean }
networkIDMeta nid = case toLower nid of
  x | x `elem` ["1", "01", "0x1", "0x01"] -> Just { friendlyName: "Ethereum Mainnet", blockExplorer: Just (etherscanBlockExplorer "https://etherscan.io"), includeInToast: true }
  x | x `elem` ["3", "03", "0x3", "0x03"] -> Just { friendlyName: "Ropsten", blockExplorer: Just (etherscanBlockExplorer "https://ropsten.etherscan.io"), includeInToast: true }
  x | x `elem` ["4", "04", "0x4", "0x04"] -> Just { friendlyName: "Rinkeby", blockExplorer: Just (etherscanBlockExplorer "https://rinkeby.etherscan.io"), includeInToast: true }
  x | x `elem` ["5", "05", "0x5", "0x05"] -> Just { friendlyName: "Goerli", blockExplorer: Just (etherscanBlockExplorer "https://goerli.etherscan.io"), includeInToast: true }
  x | x `elem` ["42", "2a", "0x2a"] -> Just { friendlyName: "Kovan", blockExplorer: Just (etherscanBlockExplorer "https://kovan.etherscan.io"), includeInToast: true }
  x | x `elem` ["420123", "6691b", "0x6691b"] -> Just { friendlyName: "Cliquebait Dreamnet", blockExplorer: Nothing, includeInToast: false }
  _ -> Nothing

makeAppEnv :: Effect AppEnv
makeAppEnv = do
  contracts <- case getRelayedContracts Nothing Just of
    Nothing -> unsafeCrashWith $ "No contracts were injected by Webpack..."
    Just rcs -> do
      parsed <- for rcs $ \relayedContract -> 
        for relayedContract.networks $ \rawNet -> do
          address <- case mkHexString rawNet.address >>= mkAddress of
            Nothing -> unsafeCrashWith $ "Unable to parse address:" <> rawNet.address
            Just addr -> pure addr
          blockNumber <- case parseBigNumber hexadecimal rawNet.blockNumber of
            Nothing -> unsafeCrashWith $ "Unable to parse deploy block number:" <> rawNet.blockNumber
            Just bn -> pure $ BlockNumber bn
          pure { address, blockNumber }
      case FO.lookup "relayableNFT" parsed of
        Nothing -> unsafeCrashWith "RelayableNFT Contract was not injected by Webpack..."
        Just relayableNFT -> pure $ { relayableNFT }
  maybeWeb3Provider <-
    case runForeignMaybe getRelayedProvider of
      Just p -> pure (Just p)
      Nothing -> case runForeignMaybe getFallbackHTTPProviderURL of
        Just u -> Just <$> httpProvider u
        Nothing -> pure Nothing
  pure $ AppEnv 
    { maybeWeb3Provider
    , contracts
    }
