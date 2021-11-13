module Lora.Env where

import Prelude

import Chanterelle.Internal.Artifact (_Deployed, _address, _network, readArtifact)
import Data.Lens (_Just, (^?))
import Effect.Aff (Aff, error, throwError)
import Types (AppEnv)
import Control.Alt ((<|>))
import Effect.Class (liftEffect)
import Data.Array (head)
import Chanterelle.Internal.Utils (withExceptT')
import DApp.Relay (mintRelayed, mintRelayed', transferRelayed, transferRelayed')
import DApp.Util (makeTxOpts)
import Contracts.RelayableNFT as RNFT
import Data.Either (Either(..), either)
import Node.Process (lookupEnv)
import Network.Ethereum.Web3 (ChainCursor(..), httpProvider, mkHexString, runWeb3, unUIntN)
import Network.Ethereum.Core.Signatures (Address, mkAddress, mkPrivateKey, privateToAddress)
import Network.Ethereum.Web3.Api (eth_getAccounts, eth_sendRawTransaction, net_version)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Network.Ethereum.Core.BigNumber (decimal, hexadecimal, parseBigNumber, unsafeToInt)
import Chanterelle.Internal.Logging (LogLevel(..), log)

readArtifacts :: Int -> Aff { rnft :: Address, ft :: Address }
readArtifacts networkID = do
  rnftPath <- liftEffect $ fromMaybe "build/RelayableNFT.json" <$> lookupEnv "RELAYABLENFT_ARTIFACT"
  ftPath <- liftEffect $ fromMaybe "build/FungibleToken.json" <$> lookupEnv "FUNGIBLETOKEN_ARTIFACT"
  withExceptT' error do
    let readAddress' addrStr = maybe (throwError $ "Couldn't make a valid address out of " <> show addrStr) pure (mkHexString addrStr >>= mkAddress)
        readArtifact' name path = do
          log Info $ "Reading " <> name <> " artifact at " <> path
          art <- readArtifact path
          let maddress = art ^? _network networkID <<< _Just <<< _Deployed <<< _Just <<< _address
          addr <- maybe (throwError $ "Couldn't find valid deploy address for chain ID " <> show networkID <> " in artifact: " <> path) pure maddress
          pure addr

    rnft <- maybe (readArtifact' "RelayableNFT" rnftPath) readAddress' =<< (liftEffect $ lookupEnv "RELAYABLENFT_ADDRESS")
    log Info $ "Using RelayableNFT address: " <> show rnft
    ft <- maybe (readArtifact' "FungibleToken" ftPath) readAddress' =<< (liftEffect $ lookupEnv "FUNGIBLETOKEN_ADDRESS")
    log Info $ "Using FungibleToken address: " <> show ft
    pure { rnft, ft }

mkEnv :: Aff AppEnv
mkEnv = do
  nodeUrl <- liftEffect $ fromMaybe "http://localhost:8545" <$> lookupEnv "NODE_URL"
  provider <- liftEffect $ httpProvider nodeUrl
  relayerPrivateKeyEnv <- liftEffect $ lookupEnv "RELAYER_PRIVATE_KEY"
  let relayerPrivateKey = mkPrivateKey =<< mkHexString =<< relayerPrivateKeyEnv
  eWeb3Env <- runWeb3 provider do
    mChainID <- net_version <#> (\chainIDString -> (parseBigNumber decimal chainIDString <|> parseBigNumber hexadecimal chainIDString))
    chainID <- maybe (throwError $ error "Couldn't parse the chain ID on this node...") pure mChainID
    let chainIDInt = unsafeToInt chainID
    log Info $ "Running on node " <> nodeUrl <> " (chain ID " <> show chainID <> ")"
    primaryAccount <- case relayerPrivateKey of
      Just prv -> do
        let addr = privateToAddress prv
        log Info $ "Using relayer account " <> show addr <> " (derived from RELAYER_PRIVATE_KEY)"
        pure { addr, isFromPrivateKey: true }
      Nothing -> eth_getAccounts <#> head >>= case _ of
        Nothing -> throwError $ error "No accounts exist on the node, and no valid RELAYER_PRIVATE_KEY was not supplied"
        Just addr -> do
          log Info $ "Using relayer account " <> show addr <> " (first account on the node)"
          log Warn $ "WE ARE NOT CHECKING IF THE ACCOUNT IS LOCKED. MAKE SURE ITS UNLOCKED :)"
          pure { addr, isFromPrivateKey: false }
    pure { primaryAccount, chainID, chainIDInt }
  web3Env <- case eWeb3Env of
    Left err -> throwError <<< error $ "Couldn't get web3 metadata: " <> show err
    Right web3Env' -> pure web3Env'
  artifacts <- readArtifacts web3Env.chainIDInt
  let rnftTxOpts = makeTxOpts { from: web3Env.primaryAccount.addr, to: artifacts.rnft }
  let getRelayNonce addr =
        RNFT.getCurrentRelayNonce rnftTxOpts Latest { addr } >>= either (throwError <<< error <<< show) (pure <<< unUIntN)
  relayActions <-
    if not web3Env.primaryAccount.isFromPrivateKey
    then pure {
          doMintRelayed: \msg -> mintRelayed msg rnftTxOpts,
          doTransferRelayed: \xfer -> transferRelayed xfer rnftTxOpts,
          getRelayNonce
        }
    else case relayerPrivateKey of
      Nothing -> throwError $ error "The impossible happened -- the supplied relayer private key spontaneously combusted"
      Just prv -> pure {
          doMintRelayed: \msg -> mintRelayed' prv msg rnftTxOpts >>= eth_sendRawTransaction,
          doTransferRelayed: \xfer -> transferRelayed' prv xfer rnftTxOpts >>= eth_sendRawTransaction,
          getRelayNonce
        }
  pure { chainID: web3Env.chainID, addresses: { fungibleToken: artifacts.ft, relayableNFT: artifacts.rnft, primaryAccount: web3Env.primaryAccount.addr }, provider, relayActions }