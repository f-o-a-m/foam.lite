module UI.Component.TokenIcon.Component
  ( TokenIconProperties
  , componentHTML
  ) where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Effect (Effect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import UI.SVG as SVG
import Web.HTML (HTMLElement)

foreign import insertTokenIconWidget :: String -> TokenIconProperties -> HTMLElement -> Effect Unit

foreign import iconDivClass :: TokenIconProperties -> String

type TokenIconProperties = {

}

componentHTML :: forall action slots m. Array (HP.IProp HTMLdiv action) -> TokenIconProperties -> HH.ComponentHTML action slots m 
componentHTML containerProps iconProps = 
  HH.div containerProps
  [ HH.div [ HP.class_ (H.ClassName $ iconDivClass iconProps) ]
    [ SVG.svg { "class": "token-icon-svg", "viewBox": "0 0 140 140", "version": "1.1", "fill": "none" }
      [ SVG.defs
        [ SVG.linearGradient { "id": "token-border-gradient-simple-2-stop" }
          [ SVG.stop { "offset": "0%", "class": "token-icon-gradient-stop-1" }
          , SVG.stop { "offset": "100%", "class": "token-icon-gradient-stop-2" }
          ]
        , SVG.linearGradient { "id": "token-border-gradient-2-1" }
          [ SVG.stop { "offset": "0%", "class": "token-icon-gradient-stop-2" }
          , SVG.stop { "offset": "33.33333333%", "class": "token-icon-gradient-stop-2" }
          , SVG.stop { "offset": "100%", "class": "token-icon-gradient-stop-1" }
          ]
        , SVG.linearGradient { "id": "token-border-gradient-3-2" }
          [ SVG.stop { "offset":"0%", "class": "token-icon-gradient-stop-3" }
          , SVG.stop { "offset":"33.33333333%", "class": "token-icon-gradient-stop-3" }
          , SVG.stop { "offset":"100%", "class": "token-icon-gradient-stop-2" }
          ]
        , SVG.linearGradient { "id": "token-border-gradient-1-3" }
          [ SVG.stop { "offset": "0%", "class": "token-icon-gradient-stop-1" }
          , SVG.stop { "offset": "33.33333333%", "class": "token-icon-gradient-stop-1" }
          , SVG.stop { "offset": "100%", "class": "token-icon-gradient-stop-3" }
          ]
        , SVG.g { "id": "token-icon-cube-base", "class": "token-icon-cube-base" }
          [ SVG.path { "fill-rule": "evenodd", "clip-rule": "evenodd", "class": "token-icon-face-top", "d": "M83.6329 58.4298L70.3542 66.096L57.0754 58.4298L70.3542 50.7637L83.6329 58.4298Z" }
          , SVG.path { "fill-rule": "evenodd", "clip-rule": "evenodd", "class": "token-icon-face-side", "d": "M70.3323 89.1019V66.0958L83.611 58.4297V81.4358L70.3323 89.1019Z" }
          , SVG.path { "fill-rule": "evenodd", "clip-rule": "evenodd", "class": "token-icon-face-front", "d": "M70.3739 89.0543V66.0481L57.0952 58.382V81.3881L70.3739 89.0543Z" }
          ]
        ]
      , SVG.g { "id": "Borders", "class": "token-icon-borders-group" }
        [ SVG.circle { "class": "token-icon-circle-border-impl"
                    , "cx": "70"
                    , "cy": "70"
                    , "r": "67.5"
                    , "fill": "transparent"
                    , "stroke-width": "5"
                    , "transform": "translate(70 70) rotate(90) translate(-70 -70)"
                    }
        , SVG.g { "id" :"Segmented-Gradient-Border"
                , "class": "token-icon-border-segmented-3-impl"
                , "transform": "translate(70 70) rotate(-90) translate(-70 -70)"
                }
          [ SVG.circle { "class": "token-icon-segmented-border-impl-stop-1"
                      , "cx": "70", "cy": "70", "r": "67.5", "stroke-width": "5"
                      , "style": "stroke: url(#token-border-gradient-2-1);", "fill": "transparent"
                      , "stroke-dasharray": "141.3716694115 424.1150082346"
                      }
          , SVG.circle { "class": "token-icon-segmented-border-impl-stop-2"
                      , "cx": "70", "cy": "70", "r": "67.5", "stroke-width": "5"
                      , "style": "stroke: url(#token-border-gradient-3-2);", "fill": "transparent"
                      , "stroke-dasharray": "141.3716694115 424.1150082346"
                      , "transform": "translate(70 70) rotate(120) translate(-70 -70)"
                      }
          , SVG.circle { "class": "token-icon-segmented-border-impl-stop-3"
                      , "cx": "70", "cy": "70", "r": "67.5", "stroke-width": "5"
                      , "style": "stroke: url(#token-border-gradient-1-3);", "fill": "transparent"
                      , "stroke-dasharray": "141.3716694115 424.1150082346"
                      , "transform": "translate(70 70) rotate(240) translate(-70 -70)"
                      }
          ]
        ]
      , SVG.g { "id": "Cubes", "class": "token-icon-cubes-group" }
        [ SVG.use { "href": "#token-icon-cube-base", "class": "token-icon-cube-left-bottom token-icon-cube-light-left", "x": "-26.666", "y": "15.33"  }
        , SVG.use { "href": "#token-icon-cube-base", "class": "token-icon-cube-center-bottom token-icon-cube-light-left", "y": "30.6666" }
        , SVG.use { "href": "#token-icon-cube-base", "class": "token-icon-cube-left-top token-icon-cube-light-left", "x": "-26.666", "y": "-15.33" }
        , SVG.use { "href": "#token-icon-cube-base", "class": "token-icon-cube-right-bottom token-icon-cube-light-right", "x": "26.666", "y": "16" }
        , SVG.use { "href": "#token-icon-cube-base", "class": "token-icon-cube-right-top token-icon-cube-light-right", "x": "26.666", "y": "-15.33" }
        , SVG.use { "href": "#token-icon-cube-base", "class": "token-icon-cube-center-top token-icon-cube-light-left", "y": "-30.6666" }
        , SVG.use { "href": "#token-icon-cube-base", "class": "token-icon-cube-center-center token-icon-cube-light-left" }
        ]
      ]
    ]

  ]