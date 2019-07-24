module Halogen.Canvas
  ( component
  , class Draw
  , draw
  , Input
  )
where

import Prelude

import CSS (CSS)
import Data.Maybe (Maybe(..), maybe)
import Data.Typelevel.Num (D2, d0, d1)
import Data.Vec (Vec, (!!))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement)
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement

-- CLASS

class Draw a where
  draw :: HTMLCanvasElement -> a -> Effect Unit

-- CONSTANTS

refLabel :: H.RefLabel
refLabel = H.RefLabel "canvas"

-- COMPONENT

type State picture =
  { input :: Input picture
  , ref :: Maybe HTMLCanvasElement
  }

type Input picture =
  { picture :: picture
  , css :: Maybe CSS
  , size :: Vec D2 Int
  }

data Action picture
  = Init
  | HandleInput (Input picture)

component
  :: forall picture query output m
   . Draw picture
  => MonadEffect m
  => H.Component HH.HTML query (Input picture) output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< HandleInput
      , initialize = Just Init
      }
  }

-- COMPONENT INIT

initialState :: forall picture . Input picture -> State picture
initialState input =
  { input
  , ref : Nothing
  }

-- COMPONENT RENDER

type HTML = forall action slots. HH.HTML action slots

render :: forall picture. State picture -> HTML
render { input : { size, css } } =
  HH.canvas $
    [ HP.ref refLabel
    , HP.width $ size!!d0
    , HP.height $ size!!d1
    ] <>
    ( maybe mempty (HC.style >>> pure) css
    )

-- COMPONENT ACTION

getCanvasElement :: forall a b c d e. HalogenM a b c d e (Maybe HTMLCanvasElement)
getCanvasElement = do
  maybeElement <- H.getHTMLElementRef refLabel
  pure (maybeElement >>= HTMLCanvasElement.fromHTMLElement)

handleAction
  :: forall picture output m
   . MonadEffect m
  => Draw picture
  => Action picture
  -> H.HalogenM (State picture) (Action picture) () output m Unit
handleAction = case _ of
  Init -> do
    maybeCanvasElement <- getCanvasElement
    H.modify_ _ { ref = maybeCanvasElement }
    { input } <- H.get
    handleAction (HandleInput input)
    pure unit
  HandleInput input -> do
    H.modify_ _ { input = input }
    { input : { picture }, ref } <- H.get
    case ref of
      Just canvasElement -> H.liftEffect $ draw canvasElement picture
      _ -> pure unit
