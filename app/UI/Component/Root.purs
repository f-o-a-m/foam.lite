module UI.Component.Root where

import Prelude

import Contracts.RelayableNFT as RNFT
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, lift)
import DApp.Message (DAppMessage(..), parseDAppMessage)
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Effect.Exception (throwException)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.EventSource (Finalizer(..))
import Halogen.Query.EventSource as ES
import Network.Ethereum.Web3 (ChainCursor(..), Change(..), EventAction(..), MultiFilterStreamState(..), UIntN, Web3, _to, defaultTransactionOptions, event', eventFilter, runWeb3, unHex)
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Ocelot.HTML.Properties (css)
import Type.Proxy (Proxy(..))
import UI.Component.Logging.Toast as Toast
import UI.Component.Map.Component (MapMessages)
import UI.Component.Map.Container as Map
import UI.Component.RelayableNFT.Table as Table
import UI.Component.RelayableNFT.Types (TableEntry(..))
import UI.Config (AppEnv(..))
import Unsafe.Coerce (unsafeCoerce)

type State = Unit

data Query a

data Action
  = Initialize
  | NewNFTEvent {change :: Change, event :: NFTEvent, message :: DAppMessage}
  | HandleMapMessages MapMessages
  | SendToastMsg Toast.ToastMsg

type Input = Unit

type Message = MapMessages

type Slots = 
  ( map :: H.Slot Map.Query MapMessages Unit
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
      HH.div_
        [ HH.div [css "left-half"] [HH.slot Map._map unit Map.component unit (Just <<< HandleMapMessages)]
        , HH.div [css "right-half"] [HH.slot Table._nftTable unit Table.component unit absurd]
        , HH.slot Toast._toast unit Toast.component unit absurd
        ]

    eval = H.mkEval H.defaultEval
             { handleAction = handleAction
             , initialize = Just Initialize

             }

    handleAction :: Action -> H.HalogenM State Action Slots Message m Unit
    handleAction = case _ of
      Initialize -> do
        AppEnv {web3Provider, contracts: {relayableNFT}} <- ask
        void $ H.subscribe $ ES.effectEventSource (\emitter -> do
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
                  Left err ->
                    ES.emit emitter $ SendToastMsg 
                      { _type: Toast.Error
                      , message: "Couldn't read tokenData for token " <> show tokenID
                      }
                  Right res -> case parseDAppMessage (BS.fromUTF8 res) of
                    Left err ->
                      ES.emit emitter $ SendToastMsg 
                        { _type: Toast.Error
                        , message: "Couldn't parse token message for token " <> show tokenID
                        }
                    Right message -> 
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
          pure $ Finalizer $ launchAff_ $ killFiber (error "Component teardown") fibre
          )
      SendToastMsg msg -> do
        _ <- H.query Toast._toast unit $ H.tell (Toast.DisplayMsg msg)
        pure unit
      NewNFTEvent {change: Change c, event, message} -> do
        let tableEntry = case event of
              NFTMint e -> Minted c.transactionHash e
              NFTTransferred e -> Transferred c.transactionHash e
        _ <- H.query Table._nftTable unit $ H.tell (Table.InsertNewTableEntry tableEntry)
        case message of
          ArbitraryString _ -> pure unit
          Location {lat,lon} -> do
            let point =
                  { coordinates: {lat, lng:lon}
                  , pointId: unHex c.transactionHash
                  }
            _ <- H.query Map._map unit $ H.tell (Map.NewPoint point)
            pure unit
          LocationWithArbitrary {lat,lon} -> do
            let point =
                  { coordinates: {lat, lng:lon}
                  , pointId: unHex c.transactionHash
                  }
            _ <- H.query Map._map unit $ H.tell (Map.NewPoint point)
            pure unit
      _ -> pure unit
