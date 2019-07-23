module Halogen.Canvas where

import Prelude

import CSS (CSS)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Typelevel.Num (D2, d0, d1)
import Data.Vec (Vec, (!!))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.DOM (Element)
import Web.HTML (HTMLElement)

-- CLASS

class Draw a ctx | a -> ctx where
  draw :: ctx -> a -> Effect Unit
  getCtx :: a -> HTMLElement -> Effect (Maybe ctx)

-- COMPONENT

type State picture ctx =
  { ctx :: Maybe ctx
  , input :: Input picture
  }

type Input picture =
  { picture :: picture
  , css :: CSS
  , size :: Vec D2 Int
  }

data Action picture
  = Init
  | HandleInput (Input picture)

type HTML = forall action slots. HH.HTML action slots

refLabel :: H.RefLabel
refLabel = H.RefLabel "canvas"

component
  :: forall picture query output ctx m
   . Draw picture ctx
  => MonadEffect m
  => H.Component HH.HTML query (Input picture) output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< HandleInput
      }
  }

-- COMPONENT INIT

initialState :: forall picture ctx. Input picture -> State picture ctx
initialState input =
  { ctx : Nothing
  , input
  }

-- COMPONENT RENDER

render :: forall picture ctx. State picture ctx -> HTML
render { input : { size, css } } =
  HH.canvas
    [ HP.ref refLabel
    , HP.width $ size!!d0
    , HP.height $ size!!d1
    ]

-- COMPONENT ACTION

handleAction
  :: forall picture ctx output m
   . MonadEffect m
  => Draw picture ctx
  => Action picture
  -> H.HalogenM (State picture ctx) (Action picture) () output m Unit
handleAction = case _ of
  Init -> do
    { input : {picture} } <- H.get
    H.getHTMLElementRef refLabel >>= traverse_ \element -> do

      maybeCtx <- H.liftEffect $ getCtx picture element
      H.modify_ $ \st -> st { ctx = maybeCtx }
      pure unit

  HandleInput input -> do
    {ctx} <- H.get
    H.modify_ \st -> st { input = input }
    case ctx of
      Just ctx' -> H.liftEffect $ draw ctx' input.picture
      Nothing -> pure unit
    pure unit
