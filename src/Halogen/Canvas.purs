module Halogen.Canvas where

import Prelude

import CSS (CSS)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (D2)
import Data.Vec (Vec)
import Effect (Effect)
import Halogen as H
import Halogen.HTML as HH
import Web.DOM (Element)

-- CLASS

class Draw a ctx where
  draw :: ctx -> a -> Effect Unit
  getCtx :: a -> Element -> Effect ctx


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

data Action
  = Init
  | GotRef Element

type HTML = forall action slots. HH.HTML action slots

component
  :: forall query picture output ctx m
   . Draw picture ctx
  => H.Component HH.HTML query (Input picture) output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Init
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
    [
    --  HH.ref (map GotRef)
    --, HP.width
    ]



-- COMPONENT ACTION

handleAction
  :: forall picture ctx output m
   . Action
  -> H.HalogenM (State picture ctx) Action () output m Unit
handleAction = case _ of
  Init -> pure unit
    --{input} <- H.get

  GotRef el -> pure unit
    --ctx <- H.liftEffect $ getContext input.picture el
    --H.modify_ \st -> st { ctx = Just ctx }
