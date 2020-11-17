module UI.Component.Root where

import Prelude

import Contracts.RelayableNFT as RNFT
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, lift)
import DApp.Message (DAppMessage(..), parseDAppMessage)
import Data.Array (catMaybes, filter, null)
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.String (joinWith)
import Effect.Aff (Aff, Milliseconds(..), delay, error, killFiber, launchAff, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as Console
import Effect.Exception (throwException)
import Effect.Timer (clearInterval, setInterval)
import Foreign.Object as FO
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (Finalizer(..))
import Halogen.Query.EventSource as ES
import Network.Ethereum.Web3 (ChainCursor(..), Change(..), EventAction(..), MultiFilterStreamState(..), UIntN, Web3, _to, defaultTransactionOptions, event', eventFilter, runWeb3, unHex)
import Network.Ethereum.Web3.Api (net_version)
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Ocelot.HTML.Properties (css)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import UI.Component.Header.Component as Header
import UI.Component.Logging.Toast as Toast
import UI.Component.Map.Component (MapMessages, Dimensions)
import UI.Component.Map.Container as Map
import UI.Component.RelayableNFT.Table as Table
import UI.Component.RelayableNFT.Types (TableEntry(..))
import UI.Config (AppEnv(..), networkIDMeta)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.ParentNode (QuerySelector(..)) as WD
import Web.Event.Event as WE
import Web.HTML as WH
import Web.HTML.HTMLElement as WH.HTMLElement
import Web.HTML.Window as WH.Window

type State = Unit

data Query a

data Action
  = Initialize
  | NewNFTEvent {change :: Change, event :: NFTEvent, message :: DAppMessage}
  | HandleMapMessages MapMessages
  | SendToastMsg Toast.ToastMsg
  | WindowResized

type Input = Unit

type Message = MapMessages

type Slots = 
  ( header :: H.Slot Header.Query Void Unit
  , map :: H.Slot Map.Query MapMessages Unit
  , nftTable :: H.Slot Table.Query Void Unit
  , toast :: H.Slot Toast.Query Void Unit
  )

data NFTEvent
  = NFTTransferred RNFT.TransferredByRelay
  | NFTMint RNFT.MintedByRelay

component
  :: ∀ m.
     MonadAff m
  => MonadAsk AppEnv m
  => H.Component HH.HTML Query Input Message m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval
    }
  where

    render :: State -> H.ComponentHTML Action Slots m
    render s =
      HH.div
        [ HP.id_ "app-root", css "flex h-full w-full flex-wrap" ]
        [ HH.slot Header._header unit Header.component unit absurd
        , HH.div
          [ css "flex flex-grow h-full w-full min-h-screen-sans-3-rem md:min-h-screen md:h-screen-sans-3-rem m-0 object-contain" ]
          [ HH.div [HP.id_ "main-left-half", css "hidden md:flex md:w-1/3 lg:w-1/2"] [HH.slot Map._map unit Map.component unit (Just <<< HandleMapMessages)]
          , HH.div [HP.id_ "main-right-half", css "flex max-w-screen w-full md:w-2/3 lg:w-1/2 overflow-x-hidden overflow-visible md:overflow-y-scroll background-gradient-event-log"] [HH.slot Table._nftTable unit Table.component unit absurd]
          ]
        , HH.slot Toast._toast unit Toast.component unit absurd
        ]

    eval = H.mkEval H.defaultEval
             { handleAction = handleAction
             , initialize = Just Initialize
             }

    handleAction :: Action -> H.HalogenM State Action Slots Message m Unit
    handleAction = case _ of
      Initialize -> do
        -- first set up our window resize listener
        window <- liftEffect WH.window
        void $ H.subscribe $ 
          ES.eventListenerEventSource (WE.EventType "resize") (WH.Window.toEventTarget window) (const $ Just WindowResized)

        -- and tell the map that its been resized to its "actual" size -- we keep retrying until 
        -- the map is initialized enough to acknowledge its correct dimensions before proceeding
        initialMapDims <- liftAff getMapDimensions
        let awaitAcknowledgedIntialResize = do
              requestAcknowledged <- H.query Map._map unit $ H.request (Map.WindowResized initialMapDims)
              if requestAcknowledged == Just true
              then do
                Console.log "Initial window resize event acknowledged"
                pure unit
              else do
                Console.log "Initial window resize event not acknowledged or ack'd with `false` -- map probably not ready yet, will retry in 25ms..."
                liftAff $ delay (Milliseconds 25.0)
                awaitAcknowledgedIntialResize

        awaitAcknowledgedIntialResize
        let sendToast m = void $ H.query Toast._toast unit $ H.tell (Toast.DisplayMsg m)
        AppEnv {maybeWeb3Provider, contracts } <- ask
        case maybeWeb3Provider of 
          Nothing -> sendToast { _type: Toast.Error, message: "No Web3 provider detected, consider installing MetaMask" }
          Just web3Provider -> do
            eNetVer <- liftAff (runWeb3 web3Provider net_version)
            case eNetVer of
              Left _ -> sendToast { _type: Toast.Error, message: "Couldn't determine Ethereum network we are connected to" }
              Right netVersion -> case FO.lookup netVersion contracts.relayableNFT of
                Nothing -> do
                  let knownNetworks = FO.keys contracts.relayableNFT
                      toastableNetworks = filter (_.includeInToast) <<< catMaybes $ networkIDMeta <$> knownNetworks 
                      toastableNetworksStr =
                        if null toastableNetworks
                        then "<looks like I was built without any artifact deployments... whoops...>"
                        else joinWith ", " (_.friendlyName <$> toastableNetworks)
                  liftEffect $ Console.log $ "known networks: " <> show knownNetworks
                  liftEffect $ Console.log $ "toastable networks: " <> show toastableNetworks
                  sendToast { _type: Toast.Error, message: "Unsupported network, please connect to one of: " <> toastableNetworksStr }
                Just rnft -> do
                  let relayableNFT = rnft.address
                      netMeta = networkIDMeta netVersion
                  case netMeta of
                    Nothing -> pure unit
                    Just { blockExplorer } -> void $ H.query Table._nftTable unit $ H.tell (Table.BlockExplorerUpdated blockExplorer)
                  
                  toastReadyNetworkName <- case netMeta of
                    Nothing -> pure "Ethereum"
                    Just { friendlyName } -> pure friendlyName
                  
                  void $ H.subscribe $ ES.effectEventSource \emitter -> do
                    -- trigger a dummy WindowResized event every 250ms, just in case a 
                    -- resize got lost somewhere along the way
                    resizeRefreshTimeout <- setInterval 250 $ do
                      ES.emit emitter $ WindowResized

                    -- now we can actually set up web3 etc
                    let 
                        filters = 
                          { mint: eventFilter (Proxy :: Proxy RNFT.MintedByRelay) relayableNFT
                          , transfer: eventFilter (Proxy :: Proxy RNFT.TransferredByRelay) relayableNFT
                          }
                        -- This is just an ad hoc way of combining both handlers in one function since they do the
                        -- same thing anyway
                        handler 
                          :: forall e a. Newtype e (Record (tokenID :: UIntN S256 | a))
                          => (Record (tokenID :: UIntN S256 | a) -> e) 
                          -> (e -> NFTEvent) 
                          -> e 
                          -> ReaderT Change Web3 EventAction
                        handler constructor actionWrapper e = do
                          Console.log "Received event"
                          Console.log $ unsafeCoerce e
                          c@(Change{blockNumber}) <- ask
                          let tokenID = (un constructor e).tokenID 
                              txOpts = defaultTransactionOptions # _to ?~ relayableNFT
                          eRes <- lift $ RNFT.tokenData txOpts (BN blockNumber) {tokenId: tokenID}
                          liftEffect case eRes of
                            Left err -> do
                              Console.log $ unsafeCoerce err
                              ES.emit emitter $ SendToastMsg 
                                { _type: Toast.Error
                                , message: "Couldn't read tokenData for token " <> show tokenID
                                }
                            Right res -> case parseDAppMessage (BS.fromUTF8 res) of
                              Left err -> do
                                Console.log "Error Decoding DAppMessage"
                                Console.log $ unsafeCoerce err
                                ES.emit emitter $ SendToastMsg 
                                  { _type: Toast.Error
                                  , message: "Couldn't parse token message for token " <> show tokenID
                                  }
                              Right message ->  do
                                Console.log "Emmitting NewNFTEvent"
                                ES.emit emitter $ NewNFTEvent 
                                  { change: c
                                  , event: actionWrapper e, message
                                  }
                          pure ContinueEvent
                        handlers = 
                          { mint: handler RNFT.MintedByRelay NFTMint
                          , transfer: handler RNFT.TransferredByRelay NFTTransferred
                          }
                    fibre <- launchAff $ do
                      ePollResult <- runWeb3 web3Provider $ event' filters handlers {windowSize:1, trailBy:0}
                      case ePollResult of
                        Left web3Error -> liftEffect do 
                          let message = "Error encountered while filtering for RelayableNFT events."
                          ES.emit emitter $ SendToastMsg $ {_type: Toast.Error, message }
                          throwException $ error $ show web3Error
                        Right result -> do
                          case result of
                            Left (MultiFilterStreamState {currentBlock}) -> 
                              Console.log $ "Polling RelayableNFT terminated by Filter at block " <> show currentBlock
                            Right receipt ->
                              Console.log $ "Polling RelayableNFT terminated by app at block " <> show receipt.blockNumber
                          liftEffect $ ES.emit emitter $ SendToastMsg {_type: Toast.Warn, message: "Event filter terminated!"}
                    ES.emit emitter $ SendToastMsg { _type: Toast.Info, message: "Monitoring " <> toastReadyNetworkName <> " for FOAM Lite transactions" }
                    pure $ Finalizer $ do
                      clearInterval resizeRefreshTimeout
                      launchAff_ $ killFiber (error "Component teardown") fibre
      SendToastMsg msg -> do
        Console.log $ "Received toast: " <> show msg._type <> " | " <> msg.message
        void $ H.query Toast._toast unit $ H.tell (Toast.DisplayMsg msg)
      NewNFTEvent {change: Change c, event, message} -> do
        let tableEntry = case event of
              NFTMint e -> Minted c.transactionHash e
              NFTTransferred e -> Transferred c.transactionHash e
        Console.log "Querying child table ..."
        void $ H.query Table._nftTable unit $ H.tell (Table.InsertNewTableEntry tableEntry)
        case message of
          ArbitraryString _ -> do
            Console.log "Got Arbitrary String"
            pure unit
          Location {lat,lon} -> do
            let point =
                  { coordinates: {lat, lng:lon}
                  , pointId: unHex c.transactionHash
                  }

            Console.log "Got Location, querying child ..."
            void $ H.query Map._map unit $ H.tell (Map.NewPoint point)
            pure unit
          LocationWithArbitrary {lat,lon} -> do
            let point =
                  { coordinates: {lat, lng:lon}
                  , pointId: unHex c.transactionHash
                  }
            Console.log "Got Location with arbitrary, querying child ..."
            _ <- H.query Map._map unit $ H.tell (Map.NewPoint point)
            pure unit
      WindowResized -> do
        dims <- liftAff getMapDimensions
        void $ H.query Map._map unit $ H.request (Map.WindowResized dims)
      (HandleMapMessages _) -> pure unit

    getMapDimensions :: Aff Dimensions
    getMapDimensions = do
      mLeftHalfElem <- HA.selectElement (WD.QuerySelector "#main-left-half")
      case mLeftHalfElem of
        Nothing -> unsafeCrashWith "Theres no #main-left-half elem containing the map! Can't handle resize events!"
        Just leftHalfElem -> do
          { width, height } <- liftEffect $ WH.HTMLElement.getBoundingClientRect leftHalfElem
          pure { width, height }