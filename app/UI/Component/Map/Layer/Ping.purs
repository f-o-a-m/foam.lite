module UI.Component.Map.Layer.Ping where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import WebMercator.LngLat (LngLat)


foreign import defaultPingProps :: forall d . PingLayerProps d
foreign import makePingLayer :: forall d . PingLayerProps d -> Layer

type PingData d =
    { position :: LngLat
    , radius :: Number
    , color :: Array Int
    | d
    }

type PingLayerProps d = BaseProps
    (currentTime :: Number
    )
    (PingData d)