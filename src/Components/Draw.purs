module Components.Draw where

import Svg.Attributes as SA
import Svg.Elements as SE
import Data.Maybe (Maybe(..))
import Halogen (Component, component, modify)
import Halogen.Component (ComponentDSL)
import Halogen.HTML (HTML())
import Halogen.HTML.Events as HE
import Prelude (class Eq, class Ord, type (~>), Unit, Void, bind, const, negate, not, pure, ($), (/))

derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

type State = { on :: Boolean }

data Input a
  = ToggleState a

data Slot = Slot

ui :: forall e. Component HTML Input Unit Void e
ui = component { initialState: const initial, render, eval, receiver: const Nothing }
  where
  initial = { on: false }

  render state =
    SE.svg [SA.viewBox x y w h]
    [ SE.circle
      [ SA.r (if state.on then w/6.0 else w/3.0)
      , SA.fill $ Just (SA.RGB 0 0 100)
      , HE.onClick (HE.input_ ToggleState)
      ]
    ]
    where
    h = 150.0
    w = 150.0
    x = -(w / 2.0)
    y = -(h / 2.0)

  eval :: Input ~> ComponentDSL State Input Void e
  eval (ToggleState next) = do
    _ <- modify (\state -> state
                { on = not state.on })
    pure next

