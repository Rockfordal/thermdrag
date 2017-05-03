module Components.Draw where

import Halogen.HTML.Events as HE
import Svg.Attributes as SA
import Svg.Elements as SE
import Data.Maybe (Maybe(Just, Nothing))
import Halogen (Component, component, modify)
import Halogen.Component (ComponentDSL)
import Halogen.HTML (HTML)
import Prelude (class Eq, class Ord, type (~>), Unit, Void, bind, const, not, pure, ($))

derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

type DSL e = ComponentDSL State Input Void e

type State = { on :: Boolean }

data Input a = ToggleState a

data Slot = Slot


ui :: forall e. Component HTML Input Unit Void e
ui = component { initialState: const initial, render, eval, receiver: const Nothing }
  where
  initial = { on: false }

  render state =
    let
      w = 1000.0
      h = 1000.0
      x = 0.0
      y = 0.0
      blue  = (SA.RGB 0 0 100)
      green = (SA.RGB 0 100 0)
    in
      SE.svg
      [ SA.viewBox x y w h
      ] [ SE.circle
          [ SA.cx 200.0
          , SA.cy 200.0
          , SA.r (if state.on then 100.0 else 150.0)
          , SA.fill $ Just (if state.on then green else blue)
          , HE.onClick (HE.input_ ToggleState)
          ]
        ]

--  , svg [ width "800", height "600"
--        , onMouseMove \e -> dispatch $ DragAt { x: e.pageX # floor, y: e.pageY # floor }
--        , onMouseUp   \e -> dispatch $ DragEnd ]

-- [ linje b1.x b1.y b2.x b2.y
-- , g' $ state.circles # mapWithIndex (viewcircle state dispatch) ]
--  circle [ onMouseDown \e -> DragStart index
--           { x: e.pageX # floor, y: e.pageY # floor } ] [ ]
--     where cc = patchcircle state index oldcircle

  eval :: Input ~> DSL e
  eval (ToggleState next) = do
    _ <- modify (\state -> state
                { on = not state.on })
    pure next

