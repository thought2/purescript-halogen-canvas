module Halogen.Canvas.Example.Main where

import Prelude

import CSS (border, px, red, solid)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Typelevel.Num (D2)
import Data.Vec (Vec, vec2)
import Effect (Effect)
import Graphics.Canvas (Context2D)
import Graphics.Canvas as GCanvas
import Halogen.Aff as HA
import Halogen.Canvas as Canvas
import Halogen.Canvas.Renderer (Renderer)
import Halogen.VDom.Driver (runUI)
import Math (pi)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement)


data Picture
  = Rect { x :: Number, y :: Number, width :: Number, height :: Number }
  | Circle { x :: Number, y :: Number, radius :: Number }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (Canvas.mkComponent cfg) input body
  where
    w = 300.0
    h = 300.0
    halve x = x / 2.0
    cfg = { renderer }
    input :: Canvas.Input (Array Picture)
    input =
      { picture :
          [ Rect
              { x: halve w
              , y : halve h
              , width : halve h * 0.9
              , height : halve h * 0.9
              }
          , Circle
              { x : halve w
              , y : halve h
              , radius : halve h
              }
          ]
      , css : Just (border solid (px 1.0) red)
      , size : vec2 w h
      }

renderer :: Renderer GCanvas.Context2D (Array Picture)
renderer =
  { init, render, onResize }
  where
    init :: Vec D2 Number -> HTMLCanvasElement -> Effect (Maybe Context2D)
    init size canvasElem =
      map Just $ GCanvas.getContext2D $ unsafeCoerce canvasElem

    render :: Context2D -> Array Picture -> Effect Unit
    render ctx pics =
      traverse_ (renderPicture ctx) pics

    renderPicture :: Context2D -> Picture -> Effect Unit
    renderPicture ctx = case _ of
      Rect opt ->
        GCanvas.strokeRect ctx opt

      Circle {x, y, radius} -> do
        GCanvas.arc ctx {x, y, radius, start : zero, end : pi * 2.0 }
        GCanvas.stroke ctx

    onResize :: Vec D2 Number -> Context2D-> Effect Context2D
    onResize size ctx =
      pure ctx
