module Routes.NullRoute where

import Prelude

import Control.Monad.Except (ExceptT)
import Data.Argonaut (class EncodeJson, encodeJson)
import Nodetrout (HTTPError)
import Text.Smolder.HTML (h1)
import Text.Smolder.Markup (text)
import Type.Trout (type (:<|>), type (:=), Resource)
import Type.Trout.ContentType.HTML (class EncodeHTML, HTML)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get)
import Types (AppM)

newtype NullRouteRes = NullRouteRes Unit
instance encodeJsonNullRouteRes :: EncodeJson NullRouteRes where
  encodeJson _ = encodeJson "Hello world!"

instance encodeHTMLNullRouteRes :: EncodeHTML NullRouteRes where
  encodeHTML _ = h1 $ text "Hello world!"

type NullRoute = "null" := Resource (Get NullRouteRes (JSON :<|> HTML))

nullRoute :: { "GET" :: ExceptT HTTPError AppM NullRouteRes }
nullRoute = { "GET": (pure (NullRouteRes unit)) }