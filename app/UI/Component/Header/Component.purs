module UI.Component.Header.Component where
  
import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.Ethereum.Web3 (runWeb3)
import Network.Ethereum.Web3.Api (net_version)
import Ocelot.HTML.Properties (css)
import UI.Monad (AppEnv(..))

_header :: SProxy "header"
_header = SProxy

data State = 
    ConnectedMainnet
  | ConnectedRinkeby
  | ConnectedCliquebait
  | UnknownNetwork
  | Disconnected

data Query a = Next a

data Action = Initialize

type Input = Unit

type Message = Void

component
  :: ∀ m.
     MonadAff m
  => MonadAsk AppEnv m
  => H.Component HH.HTML Query Input Message m
component =
  H.mkComponent
    { initialState: const Disconnected
    , render
    , eval
    }
  where

    render :: forall s. State -> H.ComponentHTML Action s m
    render s =
      HH.header
        [css "bg-navbar_dark flex flex-grow-0 flex-no-wrap relative md:fixed m-0 p-1 sm:p-2 w-full z-10 top-0 h-12 lg:h-16 object-top"]
        [ HH.div
          [css "flex mx-auto w-full h-full align-middle items-center"]
          [ HH.div
            [ css "h-full inline-flex flex-none mx-2 lg:mx-4 xl:mx-8" ]
            [ HH.img
              [ css "h-full hidden sm:inline-flex no-hover"
              , HP.alt "FOAM Logo"
              , HP.src "/foam-logo.png"
              ]
            , HH.img
              [ css "h-full inline-flex sm:hidden no-hover"
              , HP.alt "FOAM Logo"
              , HP.src "/foam-logo-small.png"
              ]
            ]
          , HH.div 
            [ HP.id_ "header-spacer"
            , css "flex-auto flex-grow h-full"
            ]
            []
          , HH.div
            [ css "inline-flex h-full mx-2 md:mx-4 lg:mx-6 xl:mx-10" ]
            [ HH.span
              [ css "inline-flex mx-auto flex-grow h-full align-middle items-center font-medium" ]
              [ HH.text stateText ]
            ]
          ]
        ]

        where stateText =
                case s of
                  ConnectedMainnet -> "Ethereum Mainnet"
                  ConnectedRinkeby -> "Rinkeby Testnet"
                  ConnectedCliquebait -> "Cliquebait Dreamnet"
                  UnknownNetwork -> "Unknown Network"
                  Disconnected -> "Disconnected"

    eval :: forall i . 
            H.HalogenQ Query Action i
         ~> H.HalogenM State Action () Message m
    eval = H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , initialize = Just Initialize
      }
      where
        handleQuery :: forall a. Query a -> H.HalogenM State Action () Message m (Maybe a)
        handleQuery (Next q) = pure (Just q)

        handleAction :: Action -> H.HalogenM State Action () Message m Unit
        handleAction = case _ of
          Initialize -> do
            AppEnv { maybeWeb3Provider } <- ask
            nextState <- case maybeWeb3Provider of
              Nothing -> pure Disconnected
              Just p -> do
                eNetID <- liftAff (runWeb3 p net_version)
                case eNetID of
                  Left _ -> pure Disconnected
                  Right netID -> do
                    liftEffect $ Console.log $ "network ID is " <> netID
                    pure $ case netID of
                      "1" -> ConnectedMainnet
                      "4" -> ConnectedRinkeby
                      "420123" -> ConnectedCliquebait
                      _ -> UnknownNetwork
            H.put nextState
