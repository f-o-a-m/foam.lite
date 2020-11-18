module UI.Component.Logging.Toast where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), isJust)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import UI.Component.Logging.Icons as Icons
import UI.Utils (css)

_toast :: SProxy "toast"
_toast = SProxy

data MsgType
  = Error
  | Warn
  | Info
  | Success

derive instance eqMsgType :: Eq MsgType 
instance showMsgType :: Show MsgType where
  show Error   = "ERROR"
  show Warn    = "WARN"
  show Info    = "INFO"
  show Success = "SUCCESS"


type ToastMsg =
  { _type :: MsgType
  , message :: String
  }


type State = 
  { toastMsg :: Maybe ToastMsg
  , previousToastMsg :: Maybe ToastMsg
  }

data Query a 
  = Clear
  | DisplayMsg ToastMsg a

data Action 
  = Dismiss

type Input = Unit

type Message = Void

component :: ∀ m . MonadAff m => H.Component HH.HTML Query Input Message m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
        { handleAction = handleAction 
        , handleQuery = handleQuery
        }
    }
  where
    pushToastMsg :: ToastMsg -> State -> State
    pushToastMsg msg st = st { toastMsg = Just msg, previousToastMsg = Nothing }

    popToastMsg :: State -> State
    popToastMsg st = st { toastMsg = Nothing, previousToastMsg = st.toastMsg }

    initialState :: State
    initialState = 
      { toastMsg: Nothing
      , previousToastMsg: Nothing
      }

    handleAction :: Action -> H.HalogenM State Action () Message m Unit
    handleAction = case _ of
      Dismiss -> H.modify_ popToastMsg

    handleQuery :: forall a.  Query a -> H.HalogenM State Action () Message m (Maybe a)
    handleQuery = case _ of
      Clear -> do
        H.modify_ popToastMsg
        pure Nothing
      DisplayMsg msg next -> do
        H.modify_ (pushToastMsg msg)
        if msg._type == Info || msg._type == Success
        then do
          H.liftAff $ delay $ Milliseconds 5000.0
          H.modify_ popToastMsg
          pure (Just next)
        else pure (Just next)

    render :: State -> H.ComponentHTML Action () m
    render state =
      let isVisible = isJust state.toastMsg
          baseContainerClasses = ["flex", "transition-all-positions", "duration-500", "ease-in-out", "items-center", "fixed", "inset-x-0", "bottom-0", "object-bottom", "z-10"]
          visibleClasses = ["mb-8"]
          hiddenClasses = ["-mb-64"]
          containerClasses = (baseContainerClasses <> (if isVisible then visibleClasses else hiddenClasses))
          toastClasses = ["flex", "rounded", "shadow-md", "mx-auto", "max-w-90p", "p-2", "sm:pr-8", "m:p-4", "items-center", "border", "border-dullergray", "bg-black"]

          toastHtml = 
            -- this is so that the toast message vanishes with the previous message intact, rather than the user briefly
            -- seeing an empty square as the toast disappears.
            -- 
            case (state.toastMsg <|> state.previousToastMsg) of
              Nothing -> []
              -- Nothing -> [ Icon.success [css "text-green text-2xl mr-2"] , HH.p_ [ HH.text "test toast mesage" ] ]
              Just s -> 
                let icon = case s._type of
                      Error -> Icons.error 6 ["hidden", "sm:inline-block"] [css "text-red text-2xl mr-2"]
                      Warn -> Icons.warning 6 ["hidden", "sm:inline-block"] [css "text-yellow text-2xl mr-2"]
                      Info -> Icons.info 6 ["hidden", "sm:inline-block"] [css "text-blue text-2xl mr-2"]
                      Success -> Icons.success  6 ["hidden", "sm:inline-block"] [css "text-green text-2xl mr-2"]
                in [ icon
                   , HH.span [css "inline-block sm:pl-4" ] [ HH.text s.message ]
                   ]
      in HH.div
           [ HP.classes (HH.ClassName <$> containerClasses)
           , HE.onClick (const $ Just $ Dismiss)
           ]
           [ HH.div [ HP.classes (HH.ClassName <$> toastClasses) ] toastHtml ]
