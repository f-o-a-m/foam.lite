module UI.Component.Icons where

-- FYI these are Tailwind's HeroIcons (https://heroicons.com)

import Prelude

import DOM.HTML.Indexed (HTMLspan)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Type.Row.Homogeneous (class Homogeneous)
import UI.Utils ((<&>))
import UI.SVG as SVG

iconHTML
  :: ∀ r i
   . HH.HTML r i
  -> Int
  -> Array String
  -> Array (HH.IProp HTMLspan i)
  -> HH.HTML r i
iconHTML svg heightProp classes iprops =
  HH.span
    [ HP.classes $ HH.ClassName <$>
      ( [ "h-" <> show heightProp
        , "w-" <> show heightProp
        ] <> classes )
    ]
    [ HH.span
        ( iprops <&>
          [ HPA.hidden "true"
          , HP.classes $ HH.ClassName <$>
            [ "h-full w-full inline"
            ]
          ]
        )
        [ svg ]
    ]

baseSVG :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
baseSVG = SVG.svg { "viewBox": "0 0 20 20", "fill": "currentColor" }

success :: ∀ p i. Int -> Array String -> Array (HH.IProp HTMLspan i) -> HH.HTML p i
success = iconHTML $ baseSVG
  [ SVG.path { "fill-rule": "evenodd", "clip-rule": "evenodd", d: "M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z"} ]

info :: ∀ p i. Int -> Array String -> Array (HH.IProp HTMLspan i) -> HH.HTML p i
info = iconHTML $ baseSVG
  [ SVG.path { "fill-rule": "evenodd", "clip-rule": "evenodd", d: "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" } ]

warning :: ∀ p i. Int -> Array String -> Array (HH.IProp HTMLspan i) -> HH.HTML p i
warning = iconHTML $ baseSVG
  [ SVG.path { "fill-rule": "evenodd", "clip-rule": "evenodd", d: "M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" } ]

error :: ∀ p i. Int -> Array String -> Array (HH.IProp HTMLspan i) -> HH.HTML p i
error = iconHTML $ baseSVG
  [ SVG.path { "fill-rule": "evenodd", "clip-rule": "evenodd", d: "M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" } ]

externalLink :: ∀ p i. Int -> Array String -> Array (HH.IProp HTMLspan i) -> HH.HTML p i
externalLink = iconHTML $ baseSVG
  [ SVG.path { d: "M11 3a1 1 0 100 2h2.586l-6.293 6.293a1 1 0 101.414 1.414L15 6.414V9a1 1 0 102 0V4a1 1 0 00-1-1h-5z" }
  , SVG.path { d: "M5 5a2 2 0 00-2 2v8a2 2 0 002 2h8a2 2 0 002-2v-3a1 1 0 10-2 0v3H5V7h3a1 1 0 000-2H5z" }
  ]