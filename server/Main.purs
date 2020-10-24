module Main where
  
import Prelude

import Chanterelle.Internal.Artifact (_Deployed, _address, _network, readArtifact)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Utils (withExceptT')
import Contracts.RelayableNFT as RNFT
import Control.Alt ((<|>))
import DApp.Relay (mintRelayed, mintRelayed', transferRelayed, transferRelayed')
import DApp.Util (makeTxOpts)
import Data.Array (head)
import Data.Either (Either(..), either)
import Data.Int (fromString) as Int
import Data.Lens (_Just, (^?))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (Aff, attempt, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Network.Ethereum.Core.BigNumber (decimal, hexadecimal, parseBigNumber, unsafeToInt)
import Network.Ethereum.Core.Signatures (Address, mkPrivateKey, privateToAddress)
import Network.Ethereum.Web3 (ChainCursor(..), httpProvider, mkHexString, runWeb3, unUIntN)
import Network.Ethereum.Web3.Api (eth_getAccounts, eth_sendRawTransaction, net_version)
import Node.HTTP as NH
import Node.Net.Socket as NNS
import Node.Process (lookupEnv)
import Node.Stream as NS
import Nodetrout (serve)
import Routes (appRoutes, routeHandlers)
import Types (AppEnv, runAppM)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = launchAff_ do
  run <- attempt do
    hostname <- liftEffect $ fromMaybe "0.0.0.0" <$> lookupEnv "SERVER_ADDRESS"
    portS <- liftEffect $ lookupEnv "SERVER_PORT"
    port <- case portS of
      Nothing -> pure 3000
      Just str -> case Int.fromString str of
        Nothing -> throwError <<< error $ "Couldn't parse " <> show portS <> " as a port number"
        Just portNumber -> pure portNumber
    env <- mkEnv
    server <- liftEffect $ NH.createServer <<< withLoggingMiddleware $ serve appRoutes routeHandlers (runAppM env) (log Error <<< show)
    liftEffect $ NH.listen server { hostname, port, backlog: Nothing } $ (log Info $ "Started listening on http://" <> hostname <> ":" <> show port)
  case run of
    Left err -> log Error $ "FATAL " <> show err
    _ -> pure unit

withLoggingMiddleware :: (NH.Request -> NH.Response -> Effect Unit) -> NH.Request -> NH.Response -> Effect Unit
withLoggingMiddleware runServer req res = do
  let method = NH.requestMethod req
      url = NH.requestURL req
      httpVersion =  (_.httpVersion <<< unsafeCoerce) req
      requestSocket = (_.socket <<< unsafeCoerce) req
      logRes level = do
        -- note that logging  statusCode/message is meaningless here, as Nodetrout uses explicit headers (i.e., calls writeHead)
        remoteAddr <- fromMaybe "<?>" <$> NNS.remoteAddress requestSocket
        remotePort <- maybe "<?>" show <$> NNS.remotePort requestSocket
        log level $ "served " <> remoteAddr <> ":" <> remotePort <> " \"" <> method <> " " <> url <> " HTTP/" <> httpVersion <> "\" "
  NS.onFinish (NH.responseAsStream res) $ logRes Info
  NS.onError (NH.responseAsStream res) $ \_ -> logRes Error
  runServer req res

readArtifacts :: Int -> Aff { rnft :: Address, ft :: Address }
readArtifacts networkID = do
  rnftPath <- liftEffect $ fromMaybe "build/RelayableNFT.json" <$> lookupEnv "RELAYABLENFT_ARTIFACT"
  ftPath <- liftEffect $ fromMaybe "build/FungibleToken.json" <$> lookupEnv "FUNGIBLETOKEN_ARTIFACT"
  withExceptT' error do
    let readArtifact' name path = do
          log Info $ "Reading " <> name <> " artifact at " <> path
          art <- readArtifact path
          let maddress = art ^? _network networkID <<< _Just <<< _Deployed <<< _Just <<< _address
          addr <- maybe (throwError $ "Couldn't find valid deploy address for chain ID " <> show networkID <> " in artifact: " <> path) pure maddress
          log Info $ "Using " <> name <> " address: " <> show addr
          pure addr
    rnft <- readArtifact' "RelayableNFT" rnftPath
    ft <- readArtifact' "FungibleToken" ftPath
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