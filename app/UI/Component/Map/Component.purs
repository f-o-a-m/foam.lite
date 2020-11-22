module UI.Component.Map.Component where

import Prelude

import Control.Lazy (fix)
import Control.Monad.Rec.Class (forever)
import Data.Array ((:), fromFoldable)
import Data.DateTime.Instant (unInstant)
import Data.Map as Map
import Data.Newtype (un, unwrap)
import Data.Tuple (snd)
import DeckGL as DeckGL
import DeckGL.BaseProps as BaseLayer
import DeckGL.Layer.Icon as Icon
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, error, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Effect.Uncurried (mkEffectFn1)
import Foreign.Object as O
import MapGL (Viewport, ClickInfo)
import MapGL as MapGL
import Math (pow)
import React as R
import Record (disjointUnion)
import UI.Component.Map.Layer.Ping as Ping
import UI.Config (baseURL)
import Unsafe.Coerce (unsafeCoerce)
import WebMercator.LngLat (LngLat)
import WebMercator.LngLat as LngLat

--------------------------------------------------------------------------------
data MapMessages
  = OnViewportChange Viewport
  | OnClick ClickInfo

data Commands
  = SetViewport' Viewport
  | AskViewport' (AVar Viewport)
  | NewPoint' MapPoint
  | RemovePing' String
  | WindowResized' Dimensions

data Messages
  = IsInitialized (Bus.BusW Commands)
  | PublicMsg MapMessages

--------------------------------------------------------------------------------

type Dimensions = { width :: Number, height :: Number }

type Props =
  { messages :: Bus.BusW Messages
  , width :: Number
  , height :: Number
  }

type State =
  { command :: Bus.BusRW Commands
  , viewport :: MapGL.Viewport
  , time :: Number
  , data :: Array MapPoint
  , pings :: Map.Map String (Ping.PingData ())
  }

mapClass :: R.ReactClass Props
mapClass = R.component "Map" \this -> do
  command <- Bus.make
  { messages, width, height } <- R.getProps this
  Milliseconds time <- unInstant <$> liftEffect now
  launchAff_ $ Bus.write (IsInitialized $ snd $ Bus.split command) messages
  pure
    { componentDidMount: componentDidMount this
    , componentWillUnmount: componentWillUnmount this
    , render: render this
    , state:
        { viewport: MapGL.Viewport
          { width: width / 2.0
          , height: height
          , longitude: -73.9738063
          , latitude: 40.6993158
          , zoom: 15.0
          , pitch: 0.0
          , bearing: 0.0
          }
        , command
        , time
        , data: []
        , pings: Map.empty
        }
    }
  where
    injectViewportDimensions :: forall r. Dimensions -> Record (viewport :: MapGL.Viewport | r) -> Record (viewport :: MapGL.Viewport | r)
    injectViewportDimensions { width: newWidth, height: newHeight } r =
      let (MapGL.Viewport oldVP) = r.viewport
          newVP = oldVP { width = newWidth, height = newHeight }
       in r { viewport = (MapGL.Viewport newVP) } 

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
          WindowResized' newDims -> liftEffect $ do
            { viewport: MapGL.Viewport { width, height } } <- R.getState this
            if width /= newDims.width || height /= newDims.height
            then R.modifyState this (injectViewportDimensions newDims)
            else pure unit
            
          SetViewport' vp -> liftEffect $ R.modifyState this _{viewport = vp}
          AskViewport' var -> liftEffect (R.getState this) >>= \{viewport} -> AVar.put viewport var
          NewPoint' p -> do 
            Console.log $ "Map Component received point " <> unsafeCoerce p
            liftEffect $ R.modifyState this \st -> st 
              { data = p : st.data
              , pings = Map.insert p.pointId (mapPointToPing p) st.pings
              }
            Bus.write (RemovePing' p.pointId) command
          RemovePing' pid -> do
            delay (Milliseconds 5000.0)
            st <- liftEffect $ R.getState this
            liftEffect $ R.writeState this st {pings = Map.delete pid st.pings}
        loop

      launchAff_ $ forever  do
        delay $ Milliseconds 100.0
        Milliseconds newTime <- unInstant <$> liftEffect now
        -- Console.log $ "Updating time to " <> show newTime
        liftEffect $ R.modifyState this _{time = newTime}
        --- seed the map values
      launchAff_ do
        delay (Milliseconds 5000.0)
        Bus.write (NewPoint' newLab) command
        Bus.write (NewPoint' jengaTower) command


    render :: R.ReactThis Props State -> R.Render
    render this = do
      { messages } <- R.getProps this
      { viewport, time, data:d, pings } <- R.getState this
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
        [ R.createLeafElement layerClass 
            { viewport
            , time
            , data:d
            , pings
            } 
        ]

newLab :: MapPoint
newLab = 
  { coordinates: 
      { lng: -73.973806
      , lat: 40.6993158
      }
  , pointId: "New Lab"
  }

jengaTower :: MapPoint
jengaTower =
  { coordinates:
      { lng: -74.009288
      , lat: 40.717795
      }
  , pointId: "54 Leonard st"
  }

--------------------------------------------------------------------------------
-- | DeckGL Component
--------------------------------------------------------------------------------

type MapPoint =
  { coordinates :: {lng :: Number, lat :: Number}
  , pointId :: String
  }

-- | Icon Layer Component
type LayerProps =
  { viewport :: MapGL.Viewport
  , data :: Array MapPoint
  , pings :: Map.Map String (Ping.PingData ())
  , time :: Number
  }

type MapState = Record ()

layerClass :: R.ReactClass LayerProps
layerClass = R.component "Layers" \this -> do
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
          pingLayer = Ping.makePingLayer $ 
                        (Ping.defaultPingProps 
                          { id = "ping-layer"
                          , data = fromFoldable $ Map.values props.pings
                          -- FIXME/KILLME: turns out `currentTime` overflows and simply 
                          -- doesn't change unless we subtract by this arbitrary timestamp
                          -- (taken at the time of the commit)
                          -- Maybe this is why the shader was written using fp64 ?
                          , currentTime = props.time - 1604023421163.0
                          , visible = true
                          , opacity = 1.0
                          , pickable = false 
                          })
          iconLayer = Icon.makeIconLayer $
                        ( Icon.defaultIconProps { id = "icon-layer"
                                                , data = map (\m -> { point: m }) props.data
                                                , pickable = true
                                                , visible = true
                                                , iconAtlas = iconUrl
                                                , iconMapping = 
                                                    O.insert "beaconIcon"
                                                      basicIcon
                                                      O.empty
                                                , opacity = 1.0
                                                , getPosition = \{point} -> pointLngLat point
                                                , getIcon = const "beaconIcon"
                                                , getSize = const $ 1.0
                                                , sizeScale = (min 1.0 $ pow 1.5 (vp.zoom - 14.0)) * 18.0 * 2.0 -- copy/pasted from foam.visualizer
                                                , onClick = mkEffectFn1 onClickPoint
                                        --        , getColor = const [256.0, 140.0, 0.0]
                                                })
      pure
        $ R.createLeafElement DeckGL.deckGL
        $ DeckGL.defaultDeckGLProps { layers = [ pingLayer, iconLayer ], viewState = vp }

pointLngLat :: MapPoint -> LngLat
pointLngLat m = LngLat.make m.coordinates

mapStyle :: String
mapStyle = "mapbox://styles/mapbox/dark-v9"

mapboxApiAccessToken :: String
mapboxApiAccessToken = "pk.eyJ1IjoiYmxpbmt5MzcxMyIsImEiOiJjamVvcXZtbGYwMXgzMzNwN2JlNGhuMHduIn0.ue2IR6wHG8b9eUoSfPhTuQ"

iconUrl :: String
iconUrl = baseURL <> "/beacon.png"

type IconEntry = 
  { x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  , mask :: Boolean
  }

basicIcon :: IconEntry
basicIcon = 
  { x: 0
  , y: 0
  , width:  36
  , height: 36
  , mask: false
  }

onClickPoint
  :: BaseLayer.PickingInfo {point :: MapPoint}
  -> Effect  Boolean
onClickPoint {object: {point}} = do 
  Console.log $ unsafeCoerce $ point.coordinates
  pure true

mapPointToPing :: MapPoint -> Ping.PingData ()
mapPointToPing p =
  { position: LngLat.make p.coordinates
  , radius: 200.0
  , color: [50, 150, 255, 125]
  }