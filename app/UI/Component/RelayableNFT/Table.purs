module UI.Component.RelayableNFT.Table where

import Prelude

import Contracts.RelayableNFT as RNFT
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array ((:), (..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Effect.Exception (throwException)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.EventSource (Finalizer(..))
import Halogen.Query.EventSource as ES
import Network.Ethereum.Web3 (Change(..), EventAction(..), MultiFilterStreamState(..), event', eventFilter, runWeb3)
import Ocelot.Block.Table as Table
import Ocelot.HTML.Properties (css)
import Type.Proxy (Proxy(..))
import UI.Component.RelayableNFT.Types (TableEntry(..), generateTableEntry, tableEntryView)
import UI.Component.Toast as Toast
import UI.Monad (AppEnv(..))
import UI.Style.Block.Backdrop as Backdrop
import UI.Style.Block.Documentation as Documentation

type State = Array TableEntry

data Query a

data Action
  = Initialize
  | InsertNewTableEntry TableEntry
  | SendToastMsg Toast.ToastMsg

type Input = Unit

type Message = Void

type Slots = ( toast :: H.Slot Toast.Query Void Unit )

component
  :: ∀ m.
     MonadAff m
  => MonadAsk AppEnv m
  => H.Component HH.HTML Query Input Message m
component =
  H.mkComponent
    { initialState: const $ map generateTableEntry (1 .. 2)
    , render
    , eval
    }
  where

    render :: State -> H.ComponentHTML Action Slots m
    render s =
      HH.div_
        [ Documentation.block_
            { header: "RelaybleNFT Events Table"
            , subheader: "Table of Mint and Transfer Events for RelaybleNFT Contract"
            }
            [ Backdrop.backdrop_
              [ renderTable ]
            ]
        , HH.slot Toast._toast unit Toast.component unit absurd
        ]
      where
        renderTable =
          Table.table_ $
            [ renderHeader
            ]
            <> renderBody

        renderHeader =
          Table.row_
            [ Table.header_ [ HH.text "Type" ]
            , Table.header_ [ HH.text "Tx Hash" ]
            , Table.header_ [ HH.text "Minter" ]
            , Table.header_ [ HH.text "Owner" ]
            , Table.header_ [ HH.text "Destination" ]
            , Table.header_ [ HH.text "Relayer" ]
            , Table.header_ [ HH.text "Token ID" ]
            ]

        renderBody =
          Table.row_ <$> ( renderData <$> s )

        renderData :: ∀ p i. TableEntry -> Array (HH.HTML p i)
        renderData entry =
          let view = tableEntryView entry
          in 
            [ Table.cell  [ css "text-left" ] [ HH.text view._type ]
            , Table.cell  [ css "text-left" ] [ HH.text view.txHash ]
            , Table.cell  [ css "text-left" ] $ 
                maybe [ HH.text "N/A" ] (\a -> [ HH.text a ]) view.minter
            , Table.cell  [ css "text-left" ] $ 
                maybe [ HH.text "N/A" ] (\a -> [ HH.text a ]) view.owner
            , Table.cell  [ css "text-left" ] $ 
                maybe [ HH.text "N/A" ] (\a -> [ HH.text a ]) view.destination
            , Table.cell  [ css "text-left" ] [ HH.text view.relayer ]
            , Table.cell  [ css "text-left" ] [ HH.text view.tokenID ]
            ]


    eval :: forall f i s. 
            MonadAff m
         => MonadAsk AppEnv m
         => H.HalogenQ f Action i
         ~> H.HalogenM State Action s Message m
    eval = H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
      where
        handleAction :: Action -> H.HalogenM State Action s Message m Unit
        handleAction = case _ of
          Initialize -> do
            AppEnv {web3Provider, contracts: {relayableNFT}} <- ask
            void $ H.subscribe $ ES.effectEventSource (\emitter -> do
              let 
                  filters = 
                    { mint: eventFilter (Proxy :: Proxy RNFT.MintedByRelay) relayableNFT
                    , transfer: eventFilter (Proxy :: Proxy RNFT.TransferredByRelay) relayableNFT
                    }
                  handlers = 
                    { mint: \e -> do
                        Change c <- ask
                        liftEffect $ ES.emit emitter $ InsertNewTableEntry $ Minted c.transactionHash e
                        pure ContinueEvent
                    , transfer: \e -> do
                        Change c <- ask
                        liftEffect $ ES.emit emitter $ InsertNewTableEntry $ Transferred c.transactionHash e
                        pure ContinueEvent
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
          InsertNewTableEntry entry -> do
            st <- H.get
            H.put $ entry : st 
          SendToastMsg _ -> pure unit