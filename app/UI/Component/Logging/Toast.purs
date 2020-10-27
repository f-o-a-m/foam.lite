module UI.Component.Logging.Toast where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Toast as Toast
import Ocelot.HTML.Properties (css)

_toast :: SProxy "toast"
_toast = SProxy

data MsgType
  = Error
  | Warn
  | Info
  | Success


type ToastMsg =
  { _type :: MsgType
  , message :: String
  }


type State = 
  { toastMsg :: Maybe ToastMsg
  }

data Query a 
  = Clear
  | DisplayMsg ToastMsg

data Action 
  = Dismiss

type Input = Unit

type Message = Void

component :: ∀ m . MonadAff m => H.Component HH.HTML Query Input Message m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
    initialState :: State
    initialState = 
      { toastMsg: Just
          { _type: Success
          , message: "Woo!"
          }
      }

    handleAction :: Action -> H.HalogenM State Action () Message m Unit
    handleAction = case _ of
      Dismiss -> do
        state <- H.get
        H.modify_ _ { toastMsg = Nothing }

    handleQuery :: forall a.  Query a -> H.HalogenM State Action () Message m (Maybe a)
    handleQuery = case _ of
      Clear -> do
        state <- H.get
        H.modify_ _ { toastMsg = Nothing }
        pure Nothing
      DisplayMsg msg -> do
        state <- H.get
        H.modify_ _ { toastMsg = Just msg }
        H.liftAff $ delay $ Milliseconds 5000.0
        H.modify_ _ { toastMsg = Nothing }
        pure Nothing

    render :: State -> H.ComponentHTML Action () m
    render state =
      let toastHtml = 
            case state.toastMsg of
              Nothing -> []
              Just s -> 
                let icon = case s._type of
                      Error -> Icon.error [css "text-red text-2xl mr-2"]
                      Warn -> Icon.error [css "text-yellow text-2xl mr-2"]
                      Info -> Icon.info [css "text-blue text-2xl mr-2"]
                      Success -> Icon.success [css "text-green text-2xl mr-2"]
                in [ icon
                   , HH.p_ [ HH.text s.message]
                   ]
      in Toast.toast
           [ Toast.visible (isJust state.toastMsg) 
           , HE.onClick $ const $ Just $ Dismiss
           ]
           toastHtml