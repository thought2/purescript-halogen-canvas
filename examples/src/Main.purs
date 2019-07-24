module Halogen.Canvas.Example.Main where

import Prelude

import CSS (border, px, red, solid)
import Data.Maybe (Maybe(..))
import Data.Vec (vec2)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.Canvas (class Draw)
import Halogen.Canvas as Canvas
import Halogen.VDom.Driver (runUI)
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Canvas.component input body
  where
    input =
      { picture : Picture
      , css : Just (border solid (px 1.0) red)
      , size : vec2 300 300
      }

data Picture = Picture

instance drawPicture :: Draw Picture where
  draw = draw

foreign import draw :: HTMLCanvasElement -> Picture -> Effect Unit
