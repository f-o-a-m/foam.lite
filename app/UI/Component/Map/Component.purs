module UI.Component.Map.Component where

import Prelude

import Control.Lazy (fix)
import Data.Newtype (un, unwrap)
import Data.Tuple (snd)
import DeckGL as DeckGL
import DeckGL.BaseProps as BaseLayer
import DeckGL.Layer.Icon as Icon
import Effect (Effect)
import Effect.Aff (error, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import Foreign.Object as O
import MapGL (Viewport, ClickInfo)
import MapGL as MapGL
import Math (pow)
import React as R
import Record (disjointUnion)
import WebMercator.LngLat (LngLat)
import WebMercator.LngLat as LngLat

--------------------------------------------------------------------------------


data MapMessages
  = OnViewportChange Viewport
  | OnClick ClickInfo

data Commands
  = SetViewport' Viewport
  | AskViewport' (AVar Viewport)

data Messages
  = IsInitialized (Bus.BusW Commands)
  | PublicMsg MapMessages

--------------------------------------------------------------------------------

type Props =
  { messages :: Bus.BusW Messages
  , width :: Number
  , height :: Number
  }

type State =
  { command :: Bus.BusRW Commands
  , viewport :: MapGL.Viewport
  }

mapClass :: R.ReactClass Props
mapClass = R.component "Map" \this -> do
  command <- Bus.make
  { messages, width, height } <- R.getProps this
  launchAff_ $ Bus.write (IsInitialized $ snd $ Bus.split command) messages
  pure
    { componentDidMount: componentDidMount this
    , componentWillUnmount: componentWillUnmount this
    , render: render this
    , state:
        { viewport: MapGL.Viewport
          { width
          , height
          , longitude: -74.00539284665783
          , latitude: 40.70544878575082
          , zoom: 10.822714855509464
          , pitch: 0.0
          , bearing: 0.0
          }
        , command
        }
    }
  where
    componentWillUnmount :: R.ReactThis Props State -> R.ComponentWillUnmount
    componentWillUnmount this = R.getState this >>= \{ command } ->
      launchAff_ $ do
        props <- liftEffect $ R.getProps this
        Bus.kill (error "kill from componentWillUnmount") command

    componentDidMount :: R.ReactThis Props State -> R.ComponentDidMount
    componentDidMount this = do
      { command } <- R.getState this
      launchAff_ $ fix \loop -> do
        msg <- Bus.read command
        case msg of
          SetViewport' vp -> liftEffect $ R.modifyState this _{viewport = vp}
          AskViewport' var -> liftEffect (R.getState this) >>= \{viewport} -> AVar.put viewport var
        loop

    render :: R.ReactThis Props State -> R.Render
    render this = do
      { messages } <- R.getProps this
      { viewport } <- R.getState this
      pure $ R.createElement MapGL.mapGL
        (un MapGL.Viewport viewport `disjointUnion`
        { onViewportChange: mkEffectFn1 $ \newVp -> do
            launchAff_ $ Bus.write (PublicMsg $ OnViewportChange newVp) messages
            void $ R.modifyState this _{viewport = newVp}
        , onClick: mkEffectFn1 $ \info -> do
            launchAff_ $ Bus.write (PublicMsg $ OnClick info) messages
        , mapStyle: mapStyle
        , mapboxApiAccessToken: mapboxApiAccessToken
        , onLoad: pure unit
        , dragRotate: true
        , touchZoom: true
        , touchRotate: true
        })
        [ R.createLeafElement iconLayerClass 
            { viewport
            , data: []
            } 
        ]


--------------------------------------------------------------------------------
-- | DeckGL Component
--------------------------------------------------------------------------------

type MapPoint =
  { coordinates :: {lng :: Number, lat :: Number}
  , pointId :: String
  }

-- | Icon Layer Component
type IconProps =
  { viewport :: MapGL.Viewport
  , data :: Array MapPoint
  }

type MapState = Record ()

iconLayerClass :: R.ReactClass IconProps
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
                                                , pickable = true
                                                , visible = true
                                                , iconAtlas = "/beacon.png"
                                                , iconMapping = 
                                                    O.insert "beacon"
                                                      { x: 0
                                                      , y: 0
                                                      , width:  36
                                                      , height: 36
                                                      , mask: true
                                                      }
                                                      O.empty
                                                , opacity = 1.0
                                                , getPosition = \{point} -> pointLngLat point
                                                , getIcon = const "beacon"
                                                , getSize = const $ 
                                                    let sizeScale = (min 1.0 $ pow 1.5 (vp.zoom - 14.0)) * 18.0 * 2.0 -- copy/pasted from foam.visualizer
                                                    in sizeScale * 1.0
                                                , onClick = mkEffectFn1 onClickPoint
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

onClickPoint
  :: BaseLayer.PickingInfo {point :: MapPoint}
  -> Effect  Boolean
onClickPoint = const $ pure true
