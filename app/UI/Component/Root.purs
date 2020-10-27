module UI.Component.Root where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Ocelot.HTML.Properties (css)
import UI.Component.Map.Component (MapMessages)
import UI.Component.Map.Container as Map
import UI.Component.RelayableNFT.Table as Table
import UI.Config (AppEnv)

type State = Unit

data Query a

data Action = 
  HandleMapMessages MapMessages

type Input = Unit

type Message = MapMessages

type Slots = 
  ( map :: H.Slot Map.Query MapMessages Unit
  , nftTable :: H.Slot Table.Query Void Unit
  )

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
        ]

    eval = H.mkEval H.defaultEval
