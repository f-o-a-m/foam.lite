module UI.SVG
  ( svg
  , path
  , defs
  , g
  , linearGradient
  , stop
  , use
  , circle
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Row.Homogeneous (class Homogeneous)

_elem :: ∀ p i r. Homogeneous r String => String -> Record r -> Array (HH.HTML p i) -> HH.HTML p i
_elem name props = HH.elementNS (H.Namespace "http://www.w3.org/2000/svg") (H.ElemName name) (_rec2Attr props)

_attr :: ∀ r i. String -> String -> HH.IProp r i
_attr k v = HP.attr (H.AttrName k) v

_attr' :: ∀ r i. Tuple String String -> HH.IProp r i
_attr' (Tuple k v) = _attr k v

_rec2Attr :: ∀ p i r. Homogeneous r String => Record r -> Array (HH.IProp p i)
_rec2Attr props = _attr' <$> FO.toUnfoldable (FO.fromHomogeneous props)

svg :: ∀ p i r. Homogeneous r String => Record r -> Array (HH.HTML p i) -> HH.HTML p i
svg = _elem "svg"

path :: ∀ p i r. Homogeneous r String => Record r -> HH.HTML p i
path props = _elem "path" props []

circle :: ∀ p i r. Homogeneous r String => Record r -> HH.HTML p i
circle props = _elem "circle" props []

defs :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
defs = _elem "defs" {}

linearGradient :: ∀ p i r. Homogeneous r String => Record r -> Array (HH.HTML p i) -> HH.HTML p i
linearGradient = _elem "linearGradient"

g :: ∀ p i r. Homogeneous r String => Record r -> Array (HH.HTML p i) -> HH.HTML p i
g = _elem "g"

stop :: ∀ p i r. Homogeneous r String => Record r -> HH.HTML p i
stop props = _elem "stop" props []

use :: ∀ p i r. Homogeneous r String => Record r -> HH.HTML p i
use props = _elem "use" props []