module UI.Component.Root where

import Prelude

import Contracts.RelayableNFT as RNFT
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, lift)
import DApp.Message (DAppMessage(..), parseDAppMessage)
import Data.Array (catMaybes, filter, null)
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Lens ((?~), (.~))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, error, killFiber, launchAff, launchAff_)
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
import Network.Ethereum.Web3 (Address, ChainCursor(..), Change(..), EventAction(..), Filter, MultiFilterStreamState(..), Provider, UIntN, Web3, _fromBlock, _to, _toBlock, defaultTransactionOptions, event', eventFilter, runWeb3, unHex)
import Network.Ethereum.Web3.Api (eth_blockNumber, net_version)
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
  | NFTBackfillStatusUpdate Table.BackfillStatus
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
  = NFTTransferred { historical :: Boolean, ev :: RNFT.TransferredByRelay }
  | NFTMint { historical :: Boolean, ev :: RNFT.MintedByRelay }

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
          [ css "flex flex-grow h-full w-full min-h-screen-sans-3-rem lg:min-h-screen lg:h-screen-sans-3-rem m-0 object-contain" ]
          [ HH.div [HP.id_ "main-left-half", css "hidden lg:flex lg:w-1/4 xl:w-1/2"] [HH.slot Map._map unit Map.component unit (Just <<< HandleMapMessages)]
          , HH.div [HP.id_ "main-right-half", css "flex max-w-screen w-full lg:w-3/4 xl:w-1/2 overflow-x-hidden overflow-visible lg:overflow-y-scroll background-gradient-event-log"] [HH.slot Table._nftTable unit Table.component unit absurd]
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
            eNetProps <- liftAff <<< runWeb3 web3Provider $ do
              netVersion <- net_version
              chainHeadBlockNumber <- eth_blockNumber
              pure { netVersion, chainHeadBlockNumber }
            case eNetProps of
              Left _ -> sendToast { _type: Toast.Error, message: "Couldn't determine Ethereum network we are connected to" }
              Right { netVersion, chainHeadBlockNumber } -> case FO.lookup netVersion contracts.relayableNFT of
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
                      relayableNFTDeployBlock = rnft.blockNumber
                      netMeta = networkIDMeta netVersion
                  case netMeta of
                    Nothing -> pure unit
                    Just { blockExplorer } -> void $ H.query Table._nftTable unit $ H.tell (Table.BlockExplorerUpdated blockExplorer)
                  
                  let toastReadyNetworkName = case netMeta of
                        Nothing -> "Ethereum"
                        Just { friendlyName } -> friendlyName
                  
                  void $ H.subscribe $ ES.effectEventSource \emitter -> do
                    -- trigger a dummy WindowResized event every 250ms, just in case a 
                    -- resize got lost somewhere along the way
                    resizeRefreshTimeout <- setInterval 250 (ES.emit emitter WindowResized)

                    -- now we can actually set up web3 etc
                    historyFiber <- mkWeb3Monitor { historical: true, emitter, relayableNFT, web3Provider, startBlock: BN relayableNFTDeployBlock, endBlock: BN chainHeadBlockNumber }
                    forwardFiber <- mkWeb3Monitor { historical: false, emitter, relayableNFT, web3Provider, startBlock: BN chainHeadBlockNumber, endBlock: Latest }

                    ES.emit emitter $ SendToastMsg { _type: Toast.Info, message: "Monitoring " <> toastReadyNetworkName <> " for FOAM Lite transactions" }
                    pure $ Finalizer $ do
                      clearInterval resizeRefreshTimeout
                      launchAff_ do
                        killFiber (error "Component teardown") historyFiber
                        killFiber (error "Component teardown") forwardFiber

      SendToastMsg msg -> do
        Console.log $ "Received toast: " <> show msg._type <> " | " <> msg.message
        void $ H.query Toast._toast unit $ H.tell (Toast.DisplayMsg msg)
      NFTBackfillStatusUpdate bfs -> do
        void $ H.query Table._nftTable unit $ H.tell (Table.BackfillStatusUpdated bfs)
      NewNFTEvent {change: Change c, event, message} -> do
        let (Tuple historical tableEntry) = case event of
              NFTMint { historical, ev } -> Tuple historical $ Minted c.transactionHash ev
              NFTTransferred { historical, ev } -> Tuple historical $ Transferred c.transactionHash ev
            tableEv = if historical then Table.InsertHistoricalTableEntry else Table.InsertNewTableEntry

        void $ H.query Table._nftTable unit $ H.tell (tableEv tableEntry)
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

type Web3MonitorOpts =
  { historical :: Boolean
  , emitter :: ES.Emitter Effect Action
  , relayableNFT :: Address
  , startBlock :: ChainCursor
  , endBlock :: ChainCursor
  , web3Provider :: Provider
  }
mkWeb3Monitor :: Web3MonitorOpts -> Effect (Fiber Unit)
mkWeb3Monitor { historical, emitter, relayableNFT, startBlock, endBlock, web3Provider } = do
  let setFilterCursors :: forall e. Filter e -> Filter e
      setFilterCursors filter = filter # _fromBlock .~ startBlock # _toBlock .~ endBlock
      filters =
        { mint: setFilterCursors $ eventFilter (Proxy :: Proxy RNFT.MintedByRelay) relayableNFT
        , transfer: setFilterCursors $ eventFilter (Proxy :: Proxy RNFT.TransferredByRelay) relayableNFT
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
        { mint: handler RNFT.MintedByRelay (\ev -> NFTMint { historical, ev } )
        , transfer: handler RNFT.TransferredByRelay (\ev -> NFTTransferred { historical, ev } )
        }
  launchAff $ do
    let windowSize = if historical then 250 else 1
    when historical $ liftEffect $ ES.emit emitter $ NFTBackfillStatusUpdate Table.BackfillRunning
    ePollResult <- runWeb3 web3Provider $ event' filters handlers { windowSize, trailBy: 0 }
    let historicalOrNew = if historical then "historical" else "new"
    case ePollResult of
      Left web3Error -> liftEffect do 
        let message = "Error encountered while filtering for " <> historicalOrNew <> " RelayableNFT events."
        when historical $ liftEffect $ ES.emit emitter $ NFTBackfillStatusUpdate (Table.BackfillErrored (show web3Error))
        ES.emit emitter $ SendToastMsg $ {_type: Toast.Error, message }
        throwException $ error $ show web3Error
      Right result -> do
        case result of
          Left (MultiFilterStreamState {currentBlock}) -> 
            Console.log $ "Polling " <> historicalOrNew <> " RelayableNFT events terminated by Filter at block " <> show currentBlock
          Right receipt -> do
            Console.log $ "Polling " <> historicalOrNew <> " RelayableNFT events terminated by app at block " <> show receipt.blockNumber
        when historical $ liftEffect $ ES.emit emitter $ NFTBackfillStatusUpdate Table.BackfillFinished
        -- liftEffect $ ES.emit emitter $ SendToastMsg {_type: Toast.Warn, message: "Event filter (" <> historicalOrNew <> ") terminated!"}