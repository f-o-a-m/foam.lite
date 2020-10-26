module UI.Component.Map.Component where

import Prelude

import Data.Newtype (unwrap)
import DeckGL as DeckGL
import DeckGL.Layer.Icon as Icon
import MapGL as MapGL
import React as R
import WebMercator.LngLat (LngLat)
import WebMercator.LngLat as LngLat


--------------------------------------------------------------------------------
-- | DeckGL Component
--------------------------------------------------------------------------------

type MapPoint =
  { coordinates :: {lng :: Number, lat :: Number}
  , pointId :: String
  }

-- | Icon Layer Component
type Props =
  { viewport :: MapGL.Viewport
  , data :: Array MapPoint
  , iconMapping :: Icon.IconMapping
  , iconAtlas :: String
  , discreteZoom :: Int
  }

type State = Record ()

iconLayerClass :: R.ReactClass Props
iconLayerClass = R.component "IconLayer" \this -> do
  props <- R.getProps this
  pure
    { render: render this
    , state: {}
    }
  where
    render this = do
      props <- R.getProps this
      state <- R.getState this

      let vp = unwrap props.viewport
          iconLayer = Icon.makeIconLayer $
                        ( Icon.defaultIconProps { id = "icon"
                                                , data = map (\m -> { point: m }) props.data
                                                , pickable = false
                                                , visible = true
                                                , iconAtlas = props.iconAtlas
                                                , iconMapping = props.iconMapping
                                                , opacity = 1.0
                                                , sizeScale = 2.0 * iconSize
                                                , getPosition = \{point} -> pointLngLat point
                                                , getIcon = const "icon"
                                                , getSize = const 1.0
                                                })
      pure
        $ R.createLeafElement DeckGL.deckGL
        $ DeckGL.defaultDeckGLProps { layers = [ iconLayer ], viewState = vp }

pointLngLat :: MapPoint -> LngLat
pointLngLat m = LngLat.make m.coordinates

-- | The base icon size.
iconSize :: Number
iconSize = 60.0

mapStyle :: String
mapStyle = "mapbox://styles/mapbox/dark-v9"

mapboxApiAccessToken :: String
mapboxApiAccessToken = "pk.eyJ1IjoiYmxpbmt5MzcxMyIsImEiOiJjamVvcXZtbGYwMXgzMzNwN2JlNGhuMHduIn0.ue2IR6wHG8b9eUoSfPhTuQ"