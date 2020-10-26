module UI.Component.Tray where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Ocelot.Block.Button as Button
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Tray as Tray
import Ocelot.HTML.Properties (css)

_tray :: SProxy "tray"
_tray = SProxy

data MsgType
  = Error
  | Warn
  | Info
  | Success


type TrayMsg =
  { _type :: MsgType
  , message :: String
  }


type State = 
  { trayMsg :: Maybe TrayMsg
  }

data Query a 
  = Clear
  | DisplayMsg TrayMsg

data Action 
  = Dismiss

type Input = Unit

type Message = Void

component :: ∀ m . H.Component HH.HTML Query Input Message m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
    initialState :: State
    initialState = 
      { trayMsg: Just
          { _type: Success
          , message: "Woo!"
          }
      }

    handleAction :: Action -> H.HalogenM State Action () Message m Unit
    handleAction = case _ of
      Dismiss -> do
        state <- H.get
        H.modify_ _ { trayMsg = Nothing }

    handleQuery :: forall a.  Query a -> H.HalogenM State Action () Message m (Maybe a)
    handleQuery = case _ of
      Clear -> do
        state <- H.get
        H.modify_ _ { trayMsg = Nothing }
        pure Nothing
      DisplayMsg msg -> do
        state <- H.get
        H.modify_ _ { trayMsg = Just msg }
        pure Nothing

    render :: State -> H.ComponentHTML Action () m
    render state =
      let trayHtml = 
            case state.trayMsg of
              Nothing -> []
              Just s -> 
                let icon = case s._type of
                      Error -> Icon.error [css "text-red"]
                      Warn -> Icon.error [css "text-yellow"]
                      Info -> Icon.info [css "text-blue"]
                      Success -> Icon.success [css "text-green"]
                in [ icon
                   , HH.p
                     [ css "mr-10" ]
                     [ HH.text s.message]
                   , Button.button
                     [ HE.onClick (const $ Just Dismiss) ]
                     [ HH.text "Dismiss" ]
                   ]
      in Tray.tray
           [ Tray.open (isJust state.trayMsg) ]
           trayHtml